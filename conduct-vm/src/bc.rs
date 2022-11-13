use crate::{
    fnc::{CallFrame, FunctionDescriptor},
    heap::Gc,
    vm::DEFAULT_CALLFRAME_LEN,
};

#[derive(Debug, Clone)]
pub struct BytecodeManager {
    pub frames: Vec<CallFrame>,
    last_frame: *mut CallFrame,
}

impl BytecodeManager {
    pub fn new() -> Self {
        Self {
            frames: Vec::with_capacity(DEFAULT_CALLFRAME_LEN),
            last_frame: std::ptr::null_mut(),
        }
    }

    pub fn begin_frame(&mut self, desc: Gc<FunctionDescriptor>, stack_len: usize) {
        let len = desc.params.len();
        self.frames.push(CallFrame::new(desc, stack_len - len - 1));

        unsafe { self.last_frame = self.frames.as_mut_ptr().add(self.frames.len() - 1) }
    }

    pub fn end_frame(&mut self) -> Option<()> {
        if self.frames.pop().is_some() {
            unsafe { self.last_frame = self.last_frame.offset(-1) }
            Some(())
        } else {
            // panic!("Frame Underflow") // TODO: frame underflow
            None
        }
    }

    pub fn last_frame(&self) -> &CallFrame {
        unsafe { &*self.last_frame }
    }

    pub fn last_frame_mut(&self) -> &mut CallFrame {
        unsafe { &mut *self.last_frame }
    }
}
