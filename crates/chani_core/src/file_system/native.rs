use crate::file_system::Fd;

use super::FileSystem;
use std::io::{Read, Seek, SeekFrom, Write};

pub struct NativeFileSystem {
    pub root: String,
    pub open_file_descriptors: Vec<Option<std::fs::File>>,
}

impl FileSystem for NativeFileSystem {
    fn new() -> Self {
        NativeFileSystem {
            root: String::new(),
            open_file_descriptors: Vec::new(),
        }
    }

    fn get_name(&self) -> &str {
        "Native File System"
    }

    fn mount(&mut self, data_path: &str) -> Result<(), std::io::Error> {
        self.root = data_path.to_string();
        self.open_file_descriptors.clear();
        if !std::path::Path::new(&self.root).exists() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Directory {} does not exist", self.root),
            ));
        }
        Ok(())
    }

    fn open(&mut self, path: &[u8]) -> Result<Fd, std::io::Error> {
        let full_path = format!("{}/{}", self.root, String::from_utf8_lossy(path));

        let file = std::fs::File::open(&full_path)?;
        let existing_slot_index = self
            .open_file_descriptors
            .iter()
            .position(|slot| slot.is_none());
        let fd = match existing_slot_index {
            Some(i) => {
                self.open_file_descriptors[i] = Some(file);
                i
            }
            None => {
                self.open_file_descriptors.push(Some(file));
                self.open_file_descriptors.len() - 1
            }
        };
        Ok(fd as Fd)
    }

    fn close(&mut self, fd: Fd) -> Result<(), std::io::Error> {
        let Some(file) = self.open_file_descriptors.get_mut(fd as usize) else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "Invalid file descriptor",
            ));
        };
        *file = None;
        Ok(())
    }

    fn read(&mut self, fd: Fd, buffer: &mut [u8]) -> Result<usize, std::io::Error> {
        let file = self.get_file(fd)?;
        file.read(buffer)
            .map_err(|e| std::io::Error::other(format!("Failed to read from file: {e}")))
    }

    fn write(&mut self, fd: Fd, buffer: &[u8]) -> Result<usize, std::io::Error> {
        let file = self.get_file(fd)?;
        file.write(buffer)
            .map_err(|e| std::io::Error::other(format!("Failed to write to file: {e}")))
    }

    fn seek(&mut self, fd: Fd, position: SeekFrom) -> Result<u64, std::io::Error> {
        let file = self.get_file(fd)?;
        file.seek(position).map_err(std::io::Error::other)
    }
}

impl NativeFileSystem {
    fn get_file(&mut self, fd: Fd) -> Result<&mut std::fs::File, std::io::Error> {
        let Some(Some(file)) = self.open_file_descriptors.get_mut(fd as usize) else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "Invalid file handle",
            ));
        };
        Ok(file)
    }
}
