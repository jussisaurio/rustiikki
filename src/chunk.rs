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
    pub line_information: Vec<(usize, usize)>, // line number, count of instructions on same line
    pub constants: Vec<Value>
}

impl Chunk {
    pub fn write(&mut self, op: Opcode, line_number: usize) -> &mut Chunk {
        self.code.push(op);

        if self.line_information.len() == 0 {
            self.line_information.push((line_number, 1));
            return self
        }

        let cur_line_idx = self.line_information.len() - 1;
        let (cur_line_number, cur_line_count) = self.line_information[cur_line_idx];
        
        if cur_line_number == line_number {
            self.line_information[cur_line_idx] = (cur_line_number, cur_line_count + 1);
        } else {
            self.line_information.push((line_number, 1));
        }

        self
    }
    pub fn new() -> Chunk {
        Chunk { code: Vec::new(), line_information: Vec::new(), constants: Vec::new() }
    }
    pub fn disassembleInstruction(&self, inst_number: usize) {
        let opcode = &self.code[inst_number as usize];
        let line_number = self.get_line(inst_number);
        match opcode {
            Opcode::Constant(idx) => println!("instruction {:?} line {:?} Constant {:?}, {:?}", inst_number, line_number, idx, self.constants[*idx as usize]),
            otherwise => println!("instruction {:?} line {:?} {:?}", inst_number, line_number, otherwise),
        }
    }
    fn get_line(&self, inst_number: usize) -> usize {
        let mut i = 0;
        for (line_number, count) in &self.line_information {
            for _ in 1..=*count {
                if i == inst_number {
                    return *line_number;
                }
                i += 1;
            }
        }

        return 0;
    }
    pub fn add_constant(&mut self, c: Value) -> &mut Chunk {
        self.constants.push(c);
        self
    }

    pub fn emit_constant(&mut self, c: Value, line: usize) {
        self.add_constant(c);
        let ind = self.constants.len() - 1;
        self.write(Opcode::Constant(ind), line);
    }
}