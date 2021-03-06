mod base;
mod rust;

pub mod tools {
    use crate::rust::*;

    pub fn compile_library_template() -> Result<(), Box<dyn std::error::Error>> {
        compile_file("./src/base.rs", "./src/templates/library_template.rs")
    }
    pub fn update_rust_library() -> Result<(), Box<dyn std::error::Error>> {
        let s = super::templates::library_template::render("/*-", "-*/");
        std::fs::copy("./src/rust.rs", "./src/rust.rs.bak")?;
        std::fs::write("./src/rust.rs", s)?;
        Ok(())
    }

    pub fn create_language_library(
        language_id: &str,
        marker_begin: &str,
        marker_end: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let render = super::templates::library_template::render(marker_begin, marker_end);
        let p = format!("./src/templates/template_{}.rs", language_id);

        let _ = std::fs::copy(&p, format!("{}.bak", &p));
        std::fs::write(p, render)?;

        let f = std::fs::read_to_string("./src/lib.rs")?;
        let lang_module = format!("pub mod template_{};", language_id);

        if !f.contains(&lang_module) {
            find_and_insert_many("./src/lib.rs", "mod_templates", &lang_module)?;
        }

        Ok(())
    }
}

pub mod templates {
    /*-insert mod_templates-*/
    pub mod template_csharp;
    pub mod library_template;
    pub mod template_html;
    pub mod template_rust;
}