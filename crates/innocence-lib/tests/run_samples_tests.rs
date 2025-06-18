use std::fs::read_dir;

use innocence_lib::run_file;

#[test]
fn run_samples() {
    let files = read_dir("../../samples").unwrap();
    for file in files {
        assert!(run_file(file.unwrap().path().to_str().unwrap()).is_ok());
    }
}
