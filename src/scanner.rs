pub(crate) struct Scanner {
    file_content: String,
}

impl Scanner {
    pub(crate) fn new(file_path: &String) -> Scanner {
        let file_content =
            std::fs::read_to_string(file_path).expect(&format!("Failed to read {}", file_path));
        Scanner { file_content }
    }

    pub(crate) fn tokens(&self) -> Vec<&str> {
        self.file_content.split_whitespace().collect()
    }
}
