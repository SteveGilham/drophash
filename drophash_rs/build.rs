use std::process::Command;
use std::env;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").ok().expect("can't find out_dir");

    Command::new("C:\\Program Files (x86)\\Windows Kits\\8.1\\bin\\x64\\rc.exe")
        .args(&["/v", "/fo", "drophash_rc.lib"]) // HACK HACK HACK
        .args(&["..\\..\\..\\..\\..\\src\\drophash.rc"])
        .current_dir(&Path::new(&out_dir))
        .status()
        .unwrap();

    println!("cargo:rustc-link-search=native={}", out_dir);
    println!("cargo:rustc-link-lib=static=drophash_rc");
}