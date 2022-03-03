#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Constant(usize), // parameter is the index of constant stored in chunk
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Equal,
    Greater,
    Less
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Null
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<Opcode>,
    pub line_information: Vec<(usize, usize)>, // (line number, column number)
    pub constants: Vec<Value>
}

impl Chunk {
    pub fn write(&mut self, op: Opcode, (line, column): (usize, usize)) -> &mut Chunk {
        self.code.push(op);

        self.line_information.push((line, column));

        self
    }
    pub fn new() -> Chunk {
        Chunk { code: Vec::new(), line_information: Vec::new(), constants: Vec::new() }
    }
    pub fn disassembleInstruction(&self, inst_number: usize) {
        let opcode = &self.code[inst_number as usize];
        let (line, column) = self.get_line_and_column(inst_number);
        match opcode {
            Opcode::Constant(idx) => println!("instruction {:?} line {:?} column {:?} Constant {:?} -- {:?}", inst_number, line, column, idx, self.constants[*idx as usize]),
            otherwise => println!("instruction {:?} line {:?} column {:?} -- {:?}", inst_number, line, column, otherwise),
        }
    }
    pub fn get_line_and_column(&self, inst_number: usize) -> (usize, usize) {
        let val = self.line_information.get(inst_number).unwrap_or(&(0,0));
        *val
    }
    pub fn add_constant(&mut self, c: Value) -> &mut Chunk {
        self.constants.push(c);
        self
    }

    pub fn emit_constant(&mut self, c: Value, line_and_column: (usize, usize)) {
        self.add_constant(c);
        let ind = self.constants.len() - 1;
        self.write(Opcode::Constant(ind), line_and_column);
    }
}