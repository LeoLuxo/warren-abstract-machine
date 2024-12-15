# ATPLT Project: Warren Abstract Machine Implementation in Rust
Léo Gamboa dos Santos, 202303477


## Looking at the code
`/src/` contains most of the source code for the project.

`/src/main.rs` is technically the entry point of the project (when running it directly), but the real "root" of the project is `/src/lib.rs`.
The comments in that file should guide you around (hopefully).

`/tests/` contains integration tests.

## Running
### Via rustup + cargo
Make sure you have a STABLE (not NIGHTLY) rust toolchain installed and set up properly, as well as cargo installed.

Running `cargo run` will run `main.rs`, which may or may not be interesting.
Make sure to go and uncomment some stuff in there for more interesting results.

Running `cargo test` will run all the tests.

### Via nix
Make sure flakes are enabled in your config.
The flake.nix here provides a default package and a dev environment.

Running `nix develop` will put you in a shell with the proper cargo set up, and you can run the commands mentioned above.

Running `nix build` / `nix run` will build/run the project.