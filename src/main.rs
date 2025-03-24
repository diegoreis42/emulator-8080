use cpu::CPU;

mod cpu;

pub fn main() {
    let mut cpu = CPU::new();
    cpu.load_file("test_files/TST8080.COM");

    // inject "out 0,a" at 0x0000 (signal to stop the test)
    cpu.memory[0x0000] = 0xD3;
    cpu.memory[0x0001] = 0x00;

    // inject "out 1,a" at 0x0005 (signal to output some characters)
    cpu.memory[0x0005] = 0xD3;
    cpu.memory[0x0006] = 0x01;
    cpu.memory[0x0007] = 0xC9;

    let mut counter = 0;
    while !cpu.finished {
        cpu.step(counter);
        counter += 1;
    }
}
