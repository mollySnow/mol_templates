fn main() -> Result<(), Box<dyn std::error::Error>> {
    mol_templates::tools::create_language_library("html", "<!--|", "|-->")
    // mol_templates::tools::update_rust_library()
    // mol_templates::tools::compile_library_template()
}
