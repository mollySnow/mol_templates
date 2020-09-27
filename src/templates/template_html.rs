extern crate glob;
extern crate inflections;
extern crate nom;

use fs::create_dir_all;
use parser::{Insert, Token};
use std::{
    error::Error,
    fs::write,
    fs::{self, read_to_string},
    path::PathBuf,
};

mod comb {
    use nom::bytes::complete::take_while1;
    use nom::IResult;

    pub fn var_name(input: &str) -> IResult<&str, &str> {
        take_while1(is_var_char)(input)
    }

    pub fn trim_ws(input: &str) -> IResult<&str, &str> {
        take_while1(is_ws)(input)
    }

    pub fn is_ws(c: char) -> bool {
        c == ' ' || c == '\r' || c == '\t' || c == '\n'
    }

    pub fn is_var_char(c: char) -> bool {
        c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9'
    }
}

#[allow(dead_code)]
mod parser {
    use super::comb;
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_until};
    use nom::sequence::delimited;
    use nom::sequence::separated_pair;
    use nom::{
        combinator::{all_consuming, rest},
        multi::*,
        IResult,
    };
    use std::{collections::HashMap, error::Error};

    //this is a new comment
    pub const MARKER_BEGIN: &str = "<!--|";
    pub const MARKER_END: &str = "|-->";

    #[derive(Debug, Clone, PartialEq)]
    pub struct Insert {
        pub key: String,
        pub indent: usize,
    }
    #[derive(Debug, Clone, PartialEq)]
    pub struct Replace {
        pub replace_this: String,
        pub with_this: String,
    }
    #[derive(Debug, Clone, PartialEq)]
    pub enum Token {
        Insert(Insert),
        ContextualInsert(Insert, String),
        Replace(Vec<Replace>),
        IgnoreBegin,
        IgnoreEnd,
        ExtractBegin(String, usize),
        ExtractEnd,
        Text(String),
    }
    fn unparsed(input: &str) -> IResult<&str, (&str, String)> {
        let (input, unparsed_tokens) =
            delimited(tag(MARKER_BEGIN), take_until(MARKER_END), tag(MARKER_END))(input)?;
        let raw_marker = format!("{}{}{}", MARKER_BEGIN, unparsed_tokens, MARKER_END);
        Ok((input, (unparsed_tokens, raw_marker)))
    }

    fn preceding(input: &str) -> IResult<&str, (&str, usize)> {
        let (input, preceding) = take_until(MARKER_BEGIN)(input)?;
        Ok((input, (preceding, preceding.len())))
    }

    fn parse_replace(input: &str) -> IResult<&str, Token> {
        let (input, (_preceding, _)) = preceding(input)?;
        let (_, (unparsed_tokens, _)) = unparsed(input)?;
        let (input, _) = tag("replace")(unparsed_tokens)?;
        let (input, _) = comb::trim_ws(input)?;
        let (input, replace_pairs) = separated_list(
            tag(" "),
            separated_pair(comb::var_name, tag(":"), comb::var_name),
        )(input)?;

        let pairs = replace_pairs
            .iter()
            .map(|(key, val)| Replace {
                replace_this: (*val).to_owned(),
                with_this: (*key).to_owned(),
            })
            .collect::<Vec<Replace>>();

        Ok((input, Token::Replace(pairs)))
    }

    fn parse_insert(input: &str) -> IResult<&str, Token> {
        let (input, (_preceding, indent)) = preceding(input)?;
        let (_, (unparsed_tokens, _raw_marker)) = unparsed(input)?;
        let (input, _) = tag("insert")(unparsed_tokens)?;
        let (input, _) = comb::trim_ws(input)?;
        let (input, key) = comb::var_name(input)?;

        Ok((
            input,
            Token::Insert(Insert {
                key: key.to_owned(),
                indent,
            }),
        ))
    }

    fn parse_contextual_insert(input: &str) -> IResult<&str, Token> {
        let (input, (_preceding, indent)) = preceding(input)?;
        let (_, (unparsed_tokens, _raw_marker)) = unparsed(input)?;
        let (input, _) = tag("insert")(unparsed_tokens)?;
        let (input, _) = comb::trim_ws(input)?;
        let (input, (context, key)) =
            separated_pair(comb::var_name, tag(":"), comb::var_name)(input)?;

        Ok((
            input,
            Token::ContextualInsert(
                Insert {
                    key: key.to_owned(),
                    indent,
                },
                context.to_owned(),
            ),
        ))
    }

    fn parse_ignore_end(input: &str) -> IResult<&str, Token> {
        let (input, (_preceding, _)) = preceding(input)?;
        let (_, (unparsed_tokens, _raw_marker)) = unparsed(input)?;
        let (input, _) = all_consuming(tag("ignore_end"))(unparsed_tokens)?;

        Ok((input, Token::IgnoreEnd))
    }

    fn parse_ignore_begin(input: &str) -> IResult<&str, Token> {
        let (input, (_preceding, _)) = preceding(input)?;
        let (_, (unparsed_tokens, _raw_marker)) = unparsed(input)?;
        let (input, _) = all_consuming(tag("ignore"))(unparsed_tokens)?;

        Ok((input, Token::IgnoreBegin))
    }

    fn parse_extract_begin(input: &str) -> IResult<&str, Token> {
        let (input, (_preceding, indent)) = preceding(input)?;
        let (_, (unparsed_tokens, _raw_marker)) = unparsed(input)?;
        let (input, _) = tag("extract")(unparsed_tokens)?;
        let (input, _) = comb::trim_ws(input)?;
        let (input, name) = comb::var_name(input)?;

        Ok((input, Token::ExtractBegin(name.to_owned(), indent)))
    }

    fn parse_extract_end(input: &str) -> IResult<&str, Token> {
        let (input, (_preceding, _)) = preceding(input)?;
        let (_, (unparsed_tokens, _raw_marker)) = unparsed(input)?;
        let (input, _) = all_consuming(tag("extract_end"))(unparsed_tokens)?;

        Ok((input, Token::ExtractEnd))
    }

    fn parse_nop(input: &str) -> IResult<&str, Token> {
        let (input, rest) = rest(input)?;
        Ok((input, Token::Text(rest.to_owned())))
    }

    pub fn parse_line(input: &str) -> IResult<&str, Token> {
        alt((
            parse_contextual_insert,
            parse_insert,
            parse_replace,
            parse_ignore_begin,
            parse_ignore_end,
            parse_extract_begin,
            parse_extract_end,
            parse_nop,
        ))(input)
    }

    pub fn tokenize(input: &str) -> Result<Vec<Token>, Box<dyn Error>> {
        let output = input
            .lines()
            .map(|line| {
                let (_, parsed) = match parse_line(line) {
                    Ok(r) => r,
                    Err(err) => panic!("error parsing line: {}", err),
                };
                parsed
            })
            .collect::<Vec<Token>>();
        Ok(output)
    }

    pub fn parse(input: &str, map: &HashMap<String, String>) -> Result<String, Box<dyn Error>> {
        let (output, _) = input.lines().into_iter().fold(
            (Vec::new(), Token::Text("".to_owned())),
            |(mut o, last_row): (Vec<String>, Token), line| {
                let (_, parsed) = match parse_line(line) {
                    Ok(res) => res,
                    Err(err) => panic!("error parsing line: {}", err),
                };

                match parsed {
                    Token::Insert(insert) => {
                        let to_insert = match map.get(&insert.key) {
                            Some(n) => n,
                            None => panic!("key not found in map: {}", insert.key),
                        };
                        let s = to_insert
                            .lines()
                            .map(|l| format!("{:i$}{}", "", l, i = insert.indent))
                            .collect::<Vec<String>>()
                            .join("\r\n");

                        o.push(s);
                        o.push(line.to_owned());
                        (o, Token::Insert(insert))
                    }
                    Token::Text(_) => match last_row {
                        Token::IgnoreBegin => (o, Token::IgnoreBegin),
                        Token::Replace(replace) => {
                            let s =
                                replace
                                    .iter()
                                    .fold(line.to_owned(), |s: String, r: &Replace| {
                                        let with_this = match map.get(&r.with_this) {
                                            Some(n) => n,
                                            None => panic!("key not found in map"),
                                        };
                                        s.replace(&r.replace_this, with_this)
                                    });
                            o.push(s);
                            (o, Token::Replace(replace))
                        }
                        _s => {
                            o.push(line.to_owned());
                            (o, _s)
                        }
                    },
                    _s => (o, _s),
                }
            },
        );
        Ok(output.join("\r\n"))
    }
}

mod compile {
    use super::Token;
    use fs::create_dir_all;
    use inflections::case::to_pascal_case;
    use inflections::case::to_snake_case;
    use std::collections::HashMap;
    use std::fs::write;
    use std::{error::Error, fs};

    fn add_to_trait(context: &String, key: &String, map: &mut HashMap<String, String>) {
        match map.get_mut(context) {
            Some(v) => {
                v.push_str(&format!("\r\n\tfn get_{}(&self) -> String;", key));
            }
            None => {
                map.insert(
                    context.to_owned(),
                    format!("\tfn get_{}(&self) -> String;", key),
                );
            }
        }
    }

    pub fn compile_header(input: Vec<Token>) -> Result<String, Box<dyn Error>> {
        let mut ar = Vec::new();
        let mut iter = input.iter();
        let mut traits: HashMap<String, String> = HashMap::new();

        while let Some(token) = iter.next() {
            match token {
                Token::ExtractBegin(_, _) => {
                    let mut ignore_stack = 0;
                    while let Some(token) = iter.next() {
                        match token {
                            Token::ExtractBegin(_, _) => ignore_stack += 1,
                            Token::ExtractEnd if ignore_stack == 0 => break,
                            Token::ExtractEnd => ignore_stack -= 1,
                            _ => (),
                        }
                    }
                }
                Token::IgnoreBegin => {
                    let mut ignore_stack = 0;
                    while let Some(token) = iter.next() {
                        match token {
                            Token::IgnoreBegin => ignore_stack += 1,
                            Token::IgnoreEnd if ignore_stack == 0 => break,
                            Token::IgnoreEnd => ignore_stack -= 1,
                            _ => (),
                        }
                    }
                }
                Token::ContextualInsert(insert, context) => {
                    add_to_trait(context, &insert.key, &mut traits);
                    ar.push(format!(
                        "{}s: Vec<impl {}>",
                        inflections::case::to_snake_case(context),
                        inflections::case::to_pascal_case(context)
                    ));
                }
                Token::Insert(i) => ar.push(format!("{}: &str", i.key.clone())),
                Token::Replace(rs) => {
                    for r in rs {
                        ar.push(format!("{}: &str", r.with_this.clone()));
                    }
                }
                _ => (),
            }
        }

        ar.sort();
        ar.dedup();

        let mut s = String::new();
        for (trait_name, methods) in traits {
            s.push_str(&format!("pub trait {} {{\r\n", to_pascal_case(&trait_name)));
            s.push_str(&methods);
            s.push_str("\r\n}\r\n")
        }

        let args = ar.join(", ");
        let s = format!("{}pub fn render({}) -> String {{", s, args);
        Ok(s)
    }
    fn handle_text(row: &str, indent: usize) -> String {
        format!("{:?}", remove_indent(indent, row))
    }

    pub fn compile_body(tokens: Vec<Token>, indent: usize) -> Result<String, Box<dyn Error>> {
        let mut v = Vec::new();
        let mut iter = tokens.iter();

        while let Some(token) = iter.next() {
            match token {
                Token::Text(s) => v.push(handle_text(s, indent)),
                Token::Replace(rs) => {
                    let replace_this = match iter.next() {
                        Some(n) => match n {
                            Token::Text(t) => t,
                            _ => panic!("placer a replace marker in the wrong place."),
                        },
                        None => panic!("placed a replace marker in the wrong place."),
                    };

                    let s = rs.iter().fold(handle_text(replace_this, indent), |s, r| {
                        format!("{}.replace(\"{}\", {})", s, r.replace_this, r.with_this)
                    });
                    v.push(s);
                }
                Token::ContextualInsert(insert, context) => {
                    let s = format!("{}s.iter().fold(Vec::new(), |mut v, p| {{ for l in p.get_{}().lines() {{ v.push(format!(\"{{:i$}}{{}}\", \"\", l, i = {}));}} v}}).join(\"\\r\\n\")", to_snake_case(context),
            to_snake_case(&insert.key), insert.indent - indent);
                    v.push(s);
                }
                Token::Insert(insert) => {
                    let s = format!("{}.lines().map(|l| format!(\"{{:i$}}{{}}\", \"\", l, i = {})).collect::<Vec<String>>().join(\"\\r\\n\")", insert.key, insert.indent - indent);
                    v.push(s);
                }
                Token::ExtractBegin(_, _) => {
                    let mut ignore_stack = 0;
                    while let Some(token) = iter.next() {
                        match token {
                            Token::ExtractBegin(_, _) => ignore_stack += 1,
                            Token::ExtractEnd if ignore_stack == 0 => break,
                            Token::ExtractEnd => ignore_stack -= 1,
                            _ => (),
                        }
                    }
                }
                Token::IgnoreBegin => {
                    let mut ignore_stack = 0;
                    while let Some(token) = iter.next() {
                        match token {
                            Token::IgnoreBegin => ignore_stack += 1,
                            Token::IgnoreEnd if ignore_stack == 0 => break,
                            Token::IgnoreEnd => ignore_stack -= 1,
                            _ => (),
                        }
                    }
                }
                _ => (),
            };
        }

        let mut curl_braces = Vec::new();
        let mut evaluations = String::new();

        for e in v {
            curl_braces.push("{}".to_owned());
            evaluations.push_str(&format!(",\r\n{}", e));
        }
        let c = curl_braces.join("\\r\\n");
        let s = format!("format!(\"{}\"{})", c, evaluations);
        Ok(s)
    }
    fn remove_indent(indent: usize, input: &str) -> String {
        if input.len() > indent {
            return input[indent..].to_owned();
        }
        input.to_owned()
    }
    pub fn handle_extract<'a, I>(
        k: Option<&String>,
        i: Option<usize>,
        module: &str,
        out_dir: &str,
        tokens: &mut I,
    ) -> Result<String, Box<dyn Error>>
    where
        I: Iterator<Item = &'a Token>,
    {
        let mut modules = String::new();
        let mut v = Vec::new();

        while let Some(token) = tokens.next() {
            match token {
                Token::ExtractBegin(key, indent) => {
                    let s = handle_extract(Some(&key), Some(*indent), module, out_dir, tokens)?;
                    modules = format!("{}{}", s, modules);
                }
                Token::ExtractEnd => {
                    if let (Some(key), Some(indent)) = (k, i) {
                        let header = compile_header(v.clone())?;
                        let body = compile_body(v.clone(), indent)?;
                        let compiled = format!("{}\r\n{}\r\n}}", header, body);
                        let path = format!("{}/{}/{}.rs", out_dir, module, key);
                        create_dir_all(format!("{}/{}", out_dir, module))?;
                        write(path, compiled)?;
                        return Ok(format!("pub mod {};\r\n{}", key, modules));
                    }
                }
                t => v.push(t.clone()),
            }
        }
        Ok(modules)
    }
}

pub fn find_and_insert_many(
    pattern: &str,
    key: &str,
    insert: &str,
) -> Result<Vec<String>, Box<dyn Error>> {
    let paths = glob::glob(pattern)?;
    let mut v = Vec::new();
    for p in paths {
        let path = p?;
        if let Some(pth) = find_and_insert(&path, key, insert).and_then(|s| s.to_str()) {
            v.push(pth.to_owned());
        }
    }
    Ok(v)
}

pub fn find_and_insert<'a>(path: &'a PathBuf, marker: &str, insert: &str) -> Option<&'a PathBuf> {
    if let Ok(s) = fs::read_to_string(path) {
        if let Some(_) = s.find(&marker) {
            let mut v = Vec::new();
            for line in s.lines() {
                let (_, token) = parser::parse_line(line).unwrap();
                match token {
                    Token::Insert(Insert { key, indent }) if key == marker => {
                        let s = insert
                            .lines()
                            .map(|l| format!("{:i$}{}", "", l, i = indent));

                        v.push(line.to_owned());
                        v.extend(s);
                    }
                    _ => v.push(line.to_owned()),
                }
            }

            return match fs::write(path, v.join("\r\n")) {
                Ok(_) => Some(path),
                Err(_) => None,
            };
        }
    }
    None
}

fn compile_template(input: &str) -> Result<String, Box<dyn Error>> {
    let f = parser::tokenize(input)?;
    let header = compile::compile_header(f.clone())?;
    let body = compile::compile_body(f, 0)?;
    Ok(format!("{}\r\n{}\r\n}}", header, body))
}

pub fn compile_file_extract(path: &str, out_dir: &str, module: &str) -> Result<(), Box<dyn Error>> {
    let input = read_to_string(path)?;
    let v = parser::tokenize(&input)?;
    let mut tokens = v.iter();

    let modules = compile::handle_extract(None, None, module, out_dir, &mut tokens)?;
    let header = compile::compile_header(v.clone())?;
    let body = compile::compile_body(v, 0)?;

    let out = format!("{}{}\r\n{}\r\n}}", modules, header, body);
    let out_path = format!("{}/{}.rs", out_dir, module);
    create_dir_all(out_dir)?;
    write(out_path, out)?;
    Ok(())
}

pub fn compile_file(path: &str, out_path: &str) -> Result<(), Box<dyn Error>> {
    let s = read_to_string(path)?;
    let template_string = compile_template(&s)?
        .chars()
        .into_iter()
        .filter(|c| c.is_ascii())
        .collect::<String>();

    write(out_path, template_string)?;
    Ok(())
}