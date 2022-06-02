const message: String = "Hello, World!";
const ident msg_block = "message1";

mindy::print(message);

let cond: bool = false;
// let cond2 = false;

if cond {
    print(message);
    msg_block.flush_to();
}

// loop {}
