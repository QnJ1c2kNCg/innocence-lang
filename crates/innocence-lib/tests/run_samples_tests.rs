use std::fs::read_dir;

use innocence_lib::run_file;

#[test]
fn run_samples() {
    let files = read_dir("../../samples").unwrap();
    for file in files {
        let file_path = file.unwrap().path();
        let file_path = file_path.to_str().unwrap();
        assert!(
            run_file(file_path).is_ok(),
            "Failed to run sample file `{}`",
            file_path
        );
    }
}
