use crate::machine::DosMachineContext;

impl super::Bios {
    pub(super) fn int09_keyboard(&mut self, _ctx: &mut DosMachineContext) {
        println!("int09_keyboard");
    }
}
