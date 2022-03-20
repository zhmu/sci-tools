extern crate scitools;

use std::env;
use byteorder::ReadBytesExt;
use std::io::Cursor;
use std::io::{Seek, SeekFrom};
use midly::{live::LiveEvent, MidiMessage, TrackEvent, TrackEventKind, TrackEventKind::Midi, PitchBend};
use midly::Smf;
use midly::num::u28;

#[derive(Debug)]
pub enum SoundError {
    IoError(std::io::Error),
    UnsupportedDigitalSample(),
}

impl From<std::io::Error> for SoundError {
    fn from(error: std::io::Error) -> Self {
       SoundError::IoError(error)
    }
}

const NUM_CHANNELS: usize = 16;
const NOTE_OFF: u8 = 0x80;
const NOTE_ON: u8 = 0x90;
const POLY_AFTER: u8 = 0xa0;
const CONTROL: u8 = 0xb0;
const PCHANGE: u8 = 0xc0;
const CHNLAFTER: u8 = 0xd0;
const PBEND: u8 = 0xe0;
const SYSEX: u8 = 0xf0;
const EOX: u8 = 0xf7;
const TIMINGOVER: u8 = 0xf8;
const ENDTRK: u8 = 0xfc;

struct Sound {
}

impl Sound {
    pub fn new() -> Self {
        Sound { }
    }

    pub fn load(&mut self, data: &[u8]) -> Result<(), SoundError> {
        let mut rdr = Cursor::new(&data);

        let sound_type = rdr.read_u8()?;
        if sound_type != 0 {
            return Err(SoundError::UnsupportedDigitalSample())
        }
        for n in 0..NUM_CHANNELS {
            let voice_count = rdr.read_u8()?;
            let voice_mask = rdr.read_u8()?;
            println!("voice {}: count {} mask {:x}", n, voice_count, voice_mask);
        }

        //let events: Vec<TrackEvent> = Vec::new();
        let mut status_byte: u8 = 0;
        let mut track = midly::Track::new();
        loop {
            if rdr.position() == 0x4245 { println!("wat dan"); break; }
            let byte = rdr.read_u8();
            if byte.is_err() { break; }

            let mut delay: u32 = byte.unwrap() as u32;
            if delay == TIMINGOVER as u32 {
                loop {
                    let value = rdr.read_u8()?;
                    if value != TIMINGOVER {
                        delay += value as u32;
                        break;
                    }
                    delay += 240;
                }
            }
            let next_byte = rdr.read_u8()?;
            if (next_byte & 0x80) != 0 {
                status_byte = next_byte;
            } else {
                rdr.seek(SeekFrom::Current(-1))?;
            }

            if status_byte == SYSEX {
                println!("{:5} sysex", delay);
                let mut num_bytes: usize = 0;
                loop {
                    let next_byte = rdr.read_u8()?;
                    if next_byte == EOX { break; }
                    num_bytes += 1;
                }
                println!("     eox, {} byte(s)", num_bytes);
                continue;
            } else if status_byte == ENDTRK {
                println!("{:5}  stop sequence", delay);
                continue;
            }

            let mut message: Option<MidiMessage> = None;
            let cmd = status_byte & 0xf0;
            let ch = status_byte & 0x0f;
            if cmd == NOTE_OFF {
                let n = rdr.read_u8()?;
                let v = rdr.read_u8()?;
                println!("{:5} #{:2} note off {} {}", delay, ch, n, v);
                message = Some(MidiMessage::NoteOn{ key: n.into(), vel: v.into() });
            } else if cmd == NOTE_ON {
                let n = rdr.read_u8()?;
                let v = rdr.read_u8()?;
                println!("{:5} #{:2} note on {} {}", delay, ch, n, v);
                message = Some(MidiMessage::NoteOff{ key: n.into(), vel: v.into() });
            } else if cmd == POLY_AFTER {
                let n = rdr.read_u8()?;
                let p = rdr.read_u8()?;
                println!("{:5} #{:2} key pressure {} {}", delay, ch, n, p);

                message = Some(MidiMessage::Aftertouch{ key: n.into(), vel: p.into() });
            } else if cmd == CONTROL {
                let c = rdr.read_u8()?;
                let s = rdr.read_u8()?;
                println!("{:5} #{:2} control {} {}", delay, ch, c, s);

                message = Some(MidiMessage::Controller{ controller: c.into(), value: s.into() });
            } else if cmd == PCHANGE {
                let p = rdr.read_u8()?;
                println!("{:5} #{:2} program change {}", delay, ch, p);
                if ch == 15 {
                    if p == 127 {
                        println!("TODO: loop point");
                        continue;
                    }
                    panic!("cue stuff");
                }
                message = Some(MidiMessage::ProgramChange{ program: p.into() });
            } else if cmd == CHNLAFTER {
                let p = rdr.read_u8()?;
                println!("{:5} #{:2} pressure after-touch {}", delay, ch, p);
                message = Some(MidiMessage::ChannelAftertouch{ vel: p.into() });
            } else if cmd == PBEND {
                let t = rdr.read_u8()?;
                let b = rdr.read_u8()?;
                println!("{:5} #{:2} pitch wheel {} {}", delay, ch, t, b);

                let bend: i16 = (t as i16) << 3 + b;
                message = Some(MidiMessage::PitchBend{ bend: PitchBend::from_int(bend) });
            } else {
                unreachable!();
            }

            if let Some(message) = message {
                let kind = TrackEventKind::Midi{ channel: ch.into(), message };
                track.push(TrackEvent{ delta: delay.into(), kind });
            }
        }

        let header = midly::Header::new(midly::Format::SingleTrack, midly::Timing::Metrical(32.into()));
        let mut smf = midly::Smf::new(header);
        smf.tracks.push(track);
        smf.save("/tmp/a.mid")?;

        Ok(())
    }
}

fn main() -> Result<(), SoundError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} data/sound.nnn", args[0]);
    }
    let sound_path = &args[1];
    let sound_data = std::fs::read(sound_path)?;

    let mut sound = Sound::new();
    if let Err(x) = sound.load(&sound_data) {
        println!("load error: {:?}", x);
    }

    Ok(())
}
