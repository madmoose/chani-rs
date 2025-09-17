pub mod native;
use std::{
    fmt::Debug,
    io::{Error, ErrorKind, SeekFrom},
};

pub type Fd = u16;

pub trait FileSystem: Send {
    fn new() -> Self
    where
        Self: Sized;

    fn mount(&mut self, data_path: &str) -> Result<(), std::io::Error>;
    fn get_name(&self) -> &str;

    fn open(&mut self, path: &[u8]) -> Result<Fd, std::io::Error>;
    fn close(&mut self, fd: Fd) -> Result<(), std::io::Error>;

    fn read(&mut self, fd: Fd, buffer: &mut [u8]) -> Result<usize, std::io::Error>;
    fn write(&mut self, fd: Fd, buffer: &[u8]) -> Result<usize, std::io::Error>;
    fn seek(&mut self, fd: Fd, position: SeekFrom) -> Result<u64, std::io::Error>;
}

impl Debug for dyn FileSystem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FileSystem: {}", self.get_name())
    }
}

#[derive(Debug)]
pub struct FileSystemManager {
    drives: Vec<Option<Box<dyn FileSystem>>>,
    fds: Vec<Option<(usize, Fd)>>,
}

impl FileSystemManager {
    pub fn new() -> Self {
        FileSystemManager {
            drives: Vec::new(),
            fds: vec![None; 20],
        }
    }

    pub fn add_drive<T: FileSystem + 'static>(
        &mut self,
        data_path: &str,
    ) -> Result<(), std::io::Error> {
        self.set_drive_at_index::<T>(self.drives.len(), data_path)
    }

    pub fn set_drive_at_index<T: FileSystem + 'static>(
        &mut self,
        index: usize,
        data_path: &str,
    ) -> Result<(), std::io::Error> {
        self.drives.resize_with(index + 1, || None);
        let mut drive = Box::new(T::new());
        drive.mount(data_path)?;
        self.drives[index] = Some(drive);
        Ok(())
    }

    pub fn get_drive(&self, index: usize) -> Option<&dyn FileSystem> {
        self.drives
            .get(index)
            .and_then(|drive| drive.as_ref())
            .map(|d| d.as_ref())
    }

    pub fn get_drive_mut(&mut self, index: usize) -> Option<&mut (dyn FileSystem + 'static)> {
        self.drives
            .get_mut(index)
            .and_then(|drive| drive.as_mut().map(|d| d.as_mut()))
    }

    fn get_drive_and_fd(&mut self, fd: Fd) -> Result<(Fd, &mut (dyn FileSystem + 'static)), Error> {
        let (drive, fd) = self
            .fds
            .get(fd as usize)
            .copied()
            .flatten()
            .ok_or(Error::from(ErrorKind::InvalidInput))?;

        let drive = self
            .get_drive_mut(drive)
            .ok_or(Error::from(ErrorKind::InvalidInput))?;

        Ok((fd, drive))
    }

    pub fn open(&mut self, path: &[u8]) -> Result<Fd, std::io::Error> {
        let drive_index = 2;

        let fd_idx = self
            .fds
            .iter()
            .enumerate()
            .find(|(_, entry)| entry.is_none())
            .ok_or(Error::from(ErrorKind::StorageFull))?
            .0;

        let drive = self
            .get_drive_mut(drive_index)
            .ok_or(Error::from(ErrorKind::InvalidInput))?;

        let fd = drive.open(path)?;

        self.fds[fd_idx] = Some((drive_index, fd));

        Ok(fd_idx as Fd)
    }

    pub fn close(&mut self, fd: Fd) -> Result<(), std::io::Error> {
        let (fd, drive) = self.get_drive_and_fd(fd)?;

        drive.close(fd)?;

        self.fds[fd as usize] = None;

        Ok(())
    }

    pub fn read(&mut self, fd: Fd, buffer: &mut [u8]) -> Result<usize, std::io::Error> {
        let (fd, drive) = self.get_drive_and_fd(fd)?;

        drive.read(fd, buffer)
    }

    pub fn write(&mut self, fd: Fd, buffer: &[u8]) -> Result<usize, std::io::Error> {
        let (fd, drive) = self.get_drive_and_fd(fd)?;

        let bytes_written = drive
            .write(fd, buffer)
            .map_err(|e| Error::other(format!("Failed to write to file: {e}")))?;

        Ok(bytes_written)
    }

    pub fn seek(&mut self, fd: Fd, position: SeekFrom) -> Result<u64, std::io::Error> {
        let (fd, drive) = self.get_drive_and_fd(fd)?;

        drive.seek(fd, position)
    }
}

impl Default for FileSystemManager {
    fn default() -> Self {
        Self::new()
    }
}
