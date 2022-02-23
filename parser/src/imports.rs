pub fn get_filepath_and_import_name(s: String) -> (String, String) {
    let s = s.trim().to_string();
    let s = s.replace("import ", "");
    let filepath = get_filepath(s.clone());
    let import_name = get_import_name(s);
    (filepath, import_name)
}

pub fn is_import(s: String) -> bool {
    s.trim().starts_with("import ")
}

pub fn is_as_import(s: String) -> bool {
    s.trim().contains(" as ")
}

fn get_import_name_from_path(s: String) -> String {
    let split: Vec<&str> = s.split('/').collect();
    let result = split[split.len() - 1].to_string();
    result.trim().to_string()
}

pub fn get_filepath(s: String) -> String {
    if !is_as_import(s.clone()) {
        s.trim().to_string()
    } else {
        let split: Vec<&str> = s.split('/').collect();
        let split_len = split.len();
        let mut path = split[0..split_len - 1].join("/");
        let filename_with_as = get_import_name_from_path(s.clone());
        let filename_vec: Vec<&str> = filename_with_as.split(" as ").collect();
        let filename: &str = filename_vec[0];
        path.push('/');
        path.push_str(filename);
        path.trim().to_string()
    }
}

fn get_as_name(s: String) -> String {
    let split: Vec<&str> = s.split(" as ").collect();
    split[split.len() - 1].to_string()
}

pub fn get_import_name(s: String) -> String {
    let filename_or_as = get_import_name_from_path(s);
    if is_as_import(filename_or_as.clone()) {
        get_as_name(filename_or_as)
    } else {
        filename_or_as.replace(".socool", "")
    }
}
