use std::collections::VecDeque;
use std::fmt::Debug;
use std::sync::{Arc, Mutex, mpsc};

use crate::color::{RGB666, RGB888};

pub type FrameSender = mpsc::Sender<Frame>;
pub type FrameReceiver = mpsc::Receiver<Frame>;

pub struct Frame {
    w: usize,
    h: usize,
    data: Vec<u8>,
}

impl Default for Frame {
    fn default() -> Self {
        Self::new(320, 200)
    }
}

impl Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Frame")
            .field("w", &self.w)
            .field("h", &self.h)
            // .field("data", &self.data)
            .finish()
    }
}

impl Frame {
    pub fn new(width: usize, height: usize) -> Frame {
        Frame {
            w: width,
            h: height,
            data: vec![0; 4 * width * height],
        }
    }

    pub fn width(&self) -> usize {
        self.w
    }

    pub fn height(&self) -> usize {
        self.h
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    #[inline]
    pub fn set_rgb888(&mut self, x: usize, y: usize, color: RGB888) {
        let offset = 4 * (self.w * y + x);

        self.data[offset + 0] = color.r;
        self.data[offset + 1] = color.g;
        self.data[offset + 2] = color.b;
        self.data[offset + 3] = 255; // Alpha
    }

    #[inline]
    pub fn set_rgb666(&mut self, x: usize, y: usize, color: RGB666) {
        self.set_rgb888(x, y, color.into());
    }
}

pub struct FrameCache {
    cache: VecDeque<Frame>,
}

impl FrameCache {
    pub fn new() -> FrameCache {
        FrameCache {
            cache: VecDeque::new(),
        }
    }

    /// Gets a frame from the frame cache width the desired width or height.
    /// If the head of the frame cache is big enough, clear and use that frame's data.
    /// If not, deallocate the head frame and allocate a new frame.
    pub fn get_frame(&mut self, width: usize, height: usize) -> Frame {
        let data_size = 4 * width * height;
        if let Some(frame) = self.cache.pop_front() {
            if frame.data.len() <= data_size {
                return Frame {
                    w: width,
                    h: height,
                    data: frame.data,
                };
            }
        }

        Frame::new(width, height)
    }

    pub fn return_frame(&mut self, frame: Frame) {
        self.cache.push_back(frame);
    }
}

pub struct FrameCacheSync {
    frame_cache: Arc<Mutex<FrameCache>>,
}

impl FrameCacheSync {
    pub fn new() -> FrameCacheSync {
        FrameCacheSync {
            frame_cache: Arc::new(Mutex::new(FrameCache::new())),
        }
    }

    pub fn get_frame(&self, width: usize, height: usize) -> Frame {
        let mut frame_cache = self.frame_cache.lock().unwrap();
        frame_cache.get_frame(width, height)
    }

    pub fn return_frame(&self, frame: Frame) {
        let mut frame_cache = self.frame_cache.lock().unwrap();
        frame_cache.return_frame(frame);
    }
}

impl Clone for FrameCacheSync {
    fn clone(&self) -> FrameCacheSync {
        FrameCacheSync {
            frame_cache: Arc::clone(&self.frame_cache),
        }
    }
}

/// A queue of frames limited to 3.
/// If the queue has more 3 frames, a push will replace the last-pushed frame.
pub struct FrameQueue {
    frames: [Option<Frame>; 3],
    head: usize,
    tail: usize,
    count: usize,
}

impl FrameQueue {
    pub fn new() -> Self {
        Self {
            frames: [None, None, None],
            head: 0,
            tail: 0,
            count: 0,
        }
    }

    fn next(index: usize) -> usize {
        (index + 1) % 3
    }

    fn prev(index: usize) -> usize {
        (index + 2) % 3
    }

    pub fn push(&mut self, frame: Frame) {
        if self.count < 3 {
            self.frames[self.tail] = Some(frame);
            self.tail = Self::next(self.tail);
            self.count += 1;
        } else {
            // Queue is full, replace the last-pushed frame
            // Move tail back to overwrite the last-pushed frame
            self.tail = Self::prev(self.tail);
            self.frames[self.tail] = Some(frame);
            self.tail = Self::next(self.tail);
        }
    }

    pub fn pop(&mut self) -> Option<Frame> {
        if self.count == 0 {
            None
        } else {
            let frame = self.frames[self.head].take();
            self.head = Self::next(self.head);
            self.count -= 1;
            frame
        }
    }
}
