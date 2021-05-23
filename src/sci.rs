pub enum ReturnType {
    Void,
    Word,
    HeapPtr,
    DblList,
    Node,
    FarPtr,
    String
}

pub enum ArgType {
    Word,
    HeapPtr,
    DblList,
    Node,
    String,
    Point,
    Rect,
}

pub struct Arg {
    pub name: &'static str,
    pub atype: ArgType,
    pub optional: bool
}

pub struct KCall {
    pub name: &'static str,
    pub rtype: ReturnType,
    pub arg: &'static [Arg],
}

static KERNEL_CALLS: [KCall; 114] = [
    /* 0x00 */ KCall{ name: "Load", rtype: ReturnType::FarPtr, arg: &[
                        Arg{ name: "ResType", atype: ArgType::Word, optional: false },
                        Arg{ name: "ResNr", atype: ArgType::Word, optional: false } ] },
    /* 0x01 */ KCall{ name: "UnLoad", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "ResType", atype: ArgType::Word, optional: false },
                        Arg{ name: "ResNr", atype: ArgType::Word, optional: false } ] },
    /* 0x02 */ KCall{ name: "ScriptID", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "ScriptNr", atype: ArgType::Word, optional: false },
                        Arg{ name: "DispatchNr", atype: ArgType::Word, optional: false } ] },
    /* 0x03 */ KCall{ name: "DisposeScript", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "ScriptNr", atype: ArgType::Word, optional: false } ] },
    /* 0x04 */ KCall{ name: "Clone", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "object", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x05 */ KCall{ name: "DisposeClone", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "object", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x06 */ KCall{ name: "IsObject", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "suspected_object", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x07 */ KCall{ name: "RespondsTo?", rtype: ReturnType::Word, arg: &[ ] },
    /* 0x08 */ KCall{ name: "DrawPic", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "PicNr", atype: ArgType::Word, optional: false },
                        Arg{ name: "Animation", atype: ArgType::Word, optional: true },
                        Arg{ name: "Flags", atype: ArgType::Word, optional: true },
                        Arg{ name: "DefaultPalette", atype: ArgType::Word, optional: true } ] },
    /* 0x09 */ KCall{ name: "Show", rtype: ReturnType::Word, arg: &[ ] },
    /* 0x0a */ KCall{ name: "PicNotValid", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "PicNotValid", atype: ArgType::Word, optional: false } ] },
    /* 0x0b */ KCall{ name: "Animate", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "ViewList", atype: ArgType::DblList, optional: true },
                        Arg{ name: "cycle", atype: ArgType::Word, optional: true } ] },
    /* 0x0c */ KCall{ name: "SetNowseen", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "ViewList", atype: ArgType::DblList, optional: false } ] },
    /* 0x0d */ KCall{ name: "NumLoops", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "object", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x0e */ KCall{ name: "NumCels", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "object", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x0f */ KCall{ name: "CelWide", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "view", atype: ArgType::Word, optional: false },
                        Arg{ name: "loop", atype: ArgType::Word, optional: false },
                        Arg{ name: "cel", atype: ArgType::Word, optional: false } ] },
    /* 0x10 */ KCall{ name: "0x10?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x11 */ KCall{ name: "DrawCel", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "view", atype: ArgType::Word, optional: false },
                        Arg{ name: "loop", atype: ArgType::Word, optional: false },
                        Arg{ name: "cel", atype: ArgType::Word, optional: false },
                        Arg{ name: "pos", atype: ArgType::Point, optional: false },
                        Arg{ name: "priority", atype: ArgType::Word, optional: false } ] },
    /* 0x12 */ KCall{ name: "AddToPic", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "picviews", atype: ArgType::DblList, optional: false } ] },
    /* 0x13 */ KCall{ name: "NewWindow", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "Boundaries", atype: ArgType::Rect, optional: false },
                        Arg{ name: "Title", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "Flags", atype: ArgType::Word, optional: false },
                        Arg{ name: "Priority", atype: ArgType::Word, optional: false },
                        Arg{ name: "FGColor", atype: ArgType::Word, optional: false },
                        Arg{ name: "BGColor", atype: ArgType::Word, optional: false } ] },
    /* 0x14 */ KCall{ name: "GetPort", rtype: ReturnType::HeapPtr, arg: &[ ] },
    /* 0x15 */ KCall{ name: "SetPort", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "NewPort", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x16 */ KCall{ name: "DisposeWindow", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "Window", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x17 */ KCall{ name: "DrawControl", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "Control", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x18 */ KCall{ name: "HiliteControl", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "Control", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x19 */ KCall{ name: "EditControl", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "Control", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "Event", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x1a */ KCall{ name: "TextSize", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "dest", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "src", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "font", atype: ArgType::Word, optional: false },
                        Arg{ name: "maxwidth", atype: ArgType::Word, optional: true } ] },
    /* 0x1b */ KCall{ name: "Display", rtype: ReturnType::Void, arg: &[ // assume not subcommand 108
                        Arg{ name: "text", atype: ArgType::String, optional: false },
                        Arg{ name: "command", atype: ArgType::Word, optional: false } ] },
    /* 0x1c */ KCall{ name: "GetEvent", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "Flags", atype: ArgType::Word, optional: false },
                        Arg{ name: "Event", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x1d */ KCall{ name: "GlobalToLocal", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "Event", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x1e */ KCall{ name: "LocalToGlobal", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "Event", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x1f */ KCall{ name: "MapKeyToDir", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "Event", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x20 */ KCall{ name: "DrawMenuBar", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "mode", atype: ArgType::Word, optional: false } ] },
    /* 0x21 */ KCall{ name: "MenuSelect", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "event", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "flag", atype: ArgType::Word, optional: true } ] },
    /* 0x22 */ KCall{ name: "AddMenu", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "title", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "content", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x23 */ KCall{ name: "DrawStatus", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "text", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x24 */ KCall{ name: "Parse", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "event", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "input", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x25 */ KCall{ name: "Said", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "said_block", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x26 */ KCall{ name: "SetSynonyms", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "list", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x27 */ KCall{ name: "HaveMouse", rtype: ReturnType::Word, arg: &[ ] },
    /* 0x28 */ KCall{ name: "SetCursor", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "resource", atype: ArgType::Word, optional: false },
                        Arg{ name: "visible", atype: ArgType::Word, optional: false },
                        Arg{ name: "coordinates", atype: ArgType::Point, optional: true } ] },
    /* 0x29 */ KCall{ name: "FOpen", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "fname", atype: ArgType::String, optional: false },
                        Arg{ name: "mode", atype: ArgType::Word, optional: false } ] },
    /* 0x2a */ KCall{ name: "FPuts", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "filehandle", atype: ArgType::Word, optional: false },
                        Arg{ name: "data", atype: ArgType::String, optional: false } ] },
    /* 0x2b */ KCall{ name: "FGets", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "data", atype: ArgType::String, optional: false },
                        Arg{ name: "maxsize", atype: ArgType::Word, optional: false },
                        Arg{ name: "handle", atype: ArgType::Word, optional: false } ] },
    /* 0x2c */ KCall{ name: "FClose", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "filehandle", atype: ArgType::Word, optional: false } ] },
    /* 0x2d */ KCall{ name: "SaveGame", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "game_id", atype: ArgType::String, optional: false },
                        Arg{ name: "save_nr", atype: ArgType::Word, optional: false },
                        Arg{ name: "save_description", atype: ArgType::String, optional: false },
                        Arg{ name: "version", atype: ArgType::String, optional: false } ] },
    /* 0x2e */ KCall{ name: "RestoreGame", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "game_id", atype: ArgType::String, optional: false },
                        Arg{ name: "save_nr", atype: ArgType::Word, optional: false },
                        Arg{ name: "version", atype: ArgType::String, optional: false } ] },
    /* 0x2f */ KCall{ name: "RestartGame", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x30 */ KCall{ name: "GameIsRestarting", rtype: ReturnType::Word, arg: &[ ] },
    /* 0x31 */ KCall{ name: "DoSound", rtype: ReturnType::Void, arg: &[ // most of these return nothing
                        Arg{ name: "action", atype: ArgType::Word, optional: false } ] },
    /* 0x32 */ KCall{ name: "NewList", rtype: ReturnType::DblList, arg: &[ ] },
    /* 0x33 */ KCall{ name: "DisposeList", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x34 */ KCall{ name: "NewNode", rtype: ReturnType::Node, arg: &[
                        Arg{ name: "value", atype: ArgType::Word, optional: false },
                        Arg{ name: "key", atype: ArgType::Word, optional: false } ] },
    /* 0x35 */ KCall{ name: "FirstNode", rtype: ReturnType::Node, arg: &[
                        Arg{ name: "list", atype: ArgType::DblList, optional: false } ] },
    /* 0x36 */ KCall{ name: "LastNode", rtype: ReturnType::Node, arg: &[
                        Arg{ name: "list", atype: ArgType::DblList, optional: false } ] },
    /* 0x37 */ KCall{ name: "EmptyList", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "list", atype: ArgType::DblList, optional: false } ] },
    /* 0x38 */ KCall{ name: "NextNode", rtype: ReturnType::Node, arg: &[
                        Arg{ name: "node", atype: ArgType::Node, optional: false } ] },
    /* 0x39 */ KCall{ name: "PrevNode", rtype: ReturnType::Node, arg: &[
                        Arg{ name: "node", atype: ArgType::Node, optional: false } ] },
    /* 0x3a */ KCall{ name: "NodeValue", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "node", atype: ArgType::Node, optional: false } ] },
    /* 0x3b */ KCall{ name: "AddAfter", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "list", atype: ArgType::DblList, optional: false },
                        Arg{ name: "ref_node", atype: ArgType::Node, optional: false },
                        Arg{ name: "node", atype: ArgType::Node, optional: false } ] },
    /* 0x3c */ KCall{ name: "AddToFront", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "list", atype: ArgType::DblList, optional: false },
                        Arg{ name: "node", atype: ArgType::Node, optional: false } ] },
    /* 0x3d */ KCall{ name: "AddToEnd", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "list", atype: ArgType::DblList, optional: false },
                        Arg{ name: "node", atype: ArgType::Node, optional: false } ] },
    /* 0x3e */ KCall{ name: "FindKey", rtype: ReturnType::Node, arg: &[
                        Arg{ name: "list", atype: ArgType::DblList, optional: false },
                        Arg{ name: "key", atype: ArgType::Word, optional: false } ] },
    /* 0x3f */ KCall{ name: "DeleteKey", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "list", atype: ArgType::DblList, optional: false },
                        Arg{ name: "key", atype: ArgType::Word, optional: false } ] },
    /* 0x40 */ KCall{ name: "Random", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "min", atype: ArgType::Word, optional: false },
                        Arg{ name: "max", atype: ArgType::Word, optional: false } ] },
    /* 0x41 */ KCall{ name: "Abs", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "value", atype: ArgType::Word, optional: false } ] },
    /* 0x42 */ KCall{ name: "Sqrt", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "value", atype: ArgType::Word, optional: false } ] },
    /* 0x43 */ KCall{ name: "GetAngle", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "origin", atype: ArgType::Point, optional: false },
                        Arg{ name: "destination", atype: ArgType::Point, optional: false } ] },
    /* 0x44 */ KCall{ name: "GetDistance", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "origin", atype: ArgType::Point, optional: false },
                        Arg{ name: "destination", atype: ArgType::Point, optional: false } ] },
    /* 0x45 */ KCall{ name: "Wait", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "ticks", atype: ArgType::Word, optional: false } ] },
    /* 0x46 */ KCall{ name: "GetTime", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "mode", atype: ArgType::Word, optional: true } ] },
    /* 0x47 */ KCall{ name: "StrEnd", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "string", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x48 */ KCall{ name: "StrCat", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "dest", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "src", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x49 */ KCall{ name: "StrCmp", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "s1", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "s2", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "width", atype: ArgType::Word, optional: true } ] },
    /* 0x4a */ KCall{ name: "StrLen", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "string", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x4b */ KCall{ name: "StrCpy", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "dest", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "src", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "width", atype: ArgType::Word, optional: true } ] },
    /* 0x4c */ KCall{ name: "Format", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "dest", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "format", atype: ArgType::String, optional: false } ] },
    /* 0x4d */ KCall{ name: "GetFarText", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "resnr", atype: ArgType::Word, optional: false },
                        Arg{ name: "stringnr", atype: ArgType::Word, optional: false },
                        Arg{ name: "dest", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x4e */ KCall{ name: "ReadNumber", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "src", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x4f */ KCall{ name: "BaseSetter", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "view_obj", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x50 */ KCall{ name: "DirLoop", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "object", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "angle", atype: ArgType::Word, optional: false } ] },
    /* 0x51 */ KCall{ name: "CanBeHere", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "obj", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "clip_list", atype: ArgType::DblList, optional: true } ] },
    /* 0x52 */ KCall{ name: "OnControl", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "map", atype: ArgType::Word, optional: false },
                        Arg{ name: "area", atype: ArgType::Word, optional: false } ] }, // Point or Rect
    /* 0x53 */ KCall{ name: "InitBresen", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "mover", atype: ArgType::HeapPtr, optional: false },
                        Arg{ name: "step_factor", atype: ArgType::Word, optional: true } ] },
    /* 0x54 */ KCall{ name: "DoBresen", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x55 */ KCall{ name: "DoAvoider", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "avoider", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x56 */ KCall{ name: "SetJump?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x57 */ KCall{ name: "SetDebug", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x58 */ KCall{ name: "InspectObj?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x59 */ KCall{ name: "ShowSends?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x5a */ KCall{ name: "ShowObjs?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x5b */ KCall{ name: "ShowFree?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x5c */ KCall{ name: "MemoryInfo", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "mode", atype: ArgType::Word, optional: false } ] },
    /* 0x5d */ KCall{ name: "StackUsage?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x5e */ KCall{ name: "Profiler?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x5f */ KCall{ name: "GetMenu", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "entry", atype: ArgType::Word, optional: false },
                        Arg{ name: "key", atype: ArgType::Word, optional: false } ] },
    /* 0x60 */ KCall{ name: "SetMenu", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "entry", atype: ArgType::Word, optional: false } ] }, // varargs
    /* 0x61 */ KCall{ name: "GetSaveFiles", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "game_id", atype: ArgType::String, optional: false },
                        Arg{ name: "strspace", atype: ArgType::String, optional: false },
                        Arg{ name: "ptrs", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x62 */ KCall{ name: "GetCWD", rtype: ReturnType::HeapPtr, arg: &[
                        Arg{ name: "address", atype: ArgType::HeapPtr, optional: false } ] },
    /* 0x63 */ KCall{ name: "CheckFreeSpace", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "path", atype: ArgType::String, optional: false } ] },
    /* 0x64 */ KCall{ name: "ValidPath?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x65 */ KCall{ name: "CoordPri?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x66 */ KCall{ name: "StrAt", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "src", atype: ArgType::String, optional: false },
                        Arg{ name: "offset", atype: ArgType::Word, optional: false },
                        Arg{ name: "replacement", atype: ArgType::Word, optional: true } ] },
    /* 0x67 */ KCall{ name: "DeviceInfo", rtype: ReturnType::Word, arg: &[ // or HeapPtr
                        Arg{ name: "sub_function", atype: ArgType::Word, optional: false },
                        Arg{ name: "string1", atype: ArgType::String, optional: false },
                        Arg{ name: "string2", atype: ArgType::String, optional: true } ] },
    /* 0x68 */ KCall{ name: "GetSaveDir", rtype: ReturnType::String, arg: &[ ] },
    /* 0x69 */ KCall{ name: "CheckSaveGame", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "game_id", atype: ArgType::String, optional: false },
                        Arg{ name: "game_nr", atype: ArgType::Word, optional: false },
                        Arg{ name: "version", atype: ArgType::String, optional: true} ] },
    /* 0x6a */ KCall{ name: "ShakeScreen", rtype: ReturnType::Word, arg: &[
                        Arg{ name: "times", atype: ArgType::String, optional: false },
                        Arg{ name: "direction", atype: ArgType::Word, optional: true } ] },
    /* 0x6b */ KCall{ name: "FlushResources?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x6c */ KCall{ name: "SinMult?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x6d */ KCall{ name: "CosMult?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x6e */ KCall{ name: "SinDiv?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x6f */ KCall{ name: "CosDiv?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x70 */ KCall{ name: "Graph?", rtype: ReturnType::Void, arg: &[ ] },
    /* 0x71 */ KCall{ name: "Joystick", rtype: ReturnType::Void, arg: &[
                        Arg{ name: "subfunction", atype: ArgType::Word, optional: false },
                        Arg{ name: "param", atype: ArgType::Word, optional: false } ] },
];

pub fn lookup_kcall(nr: u16) -> Option<&'static KCall> {
    let nr: usize = nr.into();
    if nr < KERNEL_CALLS.len() {
        Some(&KERNEL_CALLS[nr])
    } else {
        None
    }
}

pub fn does_kcall_return_void(nr: u16) -> bool {
    if let Some(kcall) = lookup_kcall(nr) {
        return match kcall.rtype {
            ReturnType::Void => { true },
            _ => { false }
        };
    }
    false
}
