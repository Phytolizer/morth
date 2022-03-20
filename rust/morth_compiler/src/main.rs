use clap::Parser;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(subcommand)]
    action: Action,
}

#[derive(clap::Subcommand)]
enum Action {
    Sim,
    Com,
}

fn main() {
    let args = Args::parse();
}
