module [
    FutureEncoder,
    FutureEncoding,
    toFutureEncoder,
    FutureEncoderFormatting,
    u8,
    u16,
    u32,
    u64,
    u128,
    i8,
    i16,
    i32,
    i64,
    i128,
    f32,
    f64,
    dec,
    bool,
    string,
    sequence,
    mapping,
    record,
    tag,
    tuple,
    custom,
    appendWith,
    append,
    SequenceWalker,
    MappingWalker,
    NamedFieldFn,
    FieldFn,
    LengthInfo,
]

FutureEncoder state := state -> state where state implements FutureEncoderFormatting

FutureEncoding implements
    toFutureEncoder : val -> FutureEncoder state where val implements FutureEncoding, state implements FutureEncoderFormatting

SequenceWalker seq state elem : seq, state, (state, elem -> state) -> state
MappingWalker map state key val : map, state, (state, key, val -> state) -> state

NamedFieldFn opaque val : opaque, Str, val -> opaque where val implements FutureEncoding
FieldFn opaque val : opaque, val -> opaque where val implements FutureEncoding

LengthInfo : [Length U64, UnknownLength]

FutureEncoderFormatting implements
    u8 : U8 -> FutureEncoder state where state implements FutureEncoderFormatting
    u16 : U16 -> FutureEncoder state where state implements FutureEncoderFormatting
    u32 : U32 -> FutureEncoder state where state implements FutureEncoderFormatting
    u64 : U64 -> FutureEncoder state where state implements FutureEncoderFormatting
    u128 : U128 -> FutureEncoder state where state implements FutureEncoderFormatting
    i8 : I8 -> FutureEncoder state where state implements FutureEncoderFormatting
    i16 : I16 -> FutureEncoder state where state implements FutureEncoderFormatting
    i32 : I32 -> FutureEncoder state where state implements FutureEncoderFormatting
    i64 : I64 -> FutureEncoder state where state implements FutureEncoderFormatting
    i128 : I128 -> FutureEncoder state where state implements FutureEncoderFormatting
    f32 : F32 -> FutureEncoder state where state implements FutureEncoderFormatting
    f64 : F64 -> FutureEncoder state where state implements FutureEncoderFormatting
    dec : Dec -> FutureEncoder state where state implements FutureEncoderFormatting
    bool : Bool -> FutureEncoder state where state implements FutureEncoderFormatting
    string : Str -> FutureEncoder state where state implements FutureEncoderFormatting
    sequence :
        seq,
        LengthInfo,
        SequenceWalker seq state elem,
        (elem -> FutureEncoder state)
        -> FutureEncoder state where state implements FutureEncoderFormatting
    mapping :
        map,
        LengthInfo,
        MappingWalker map state key val,
        (key -> FutureEncoder state),
        (val -> FutureEncoder state)
        -> FutureEncoder state where state implements FutureEncoderFormatting

    # Note, the U64s below are the number of fields.
    # record, tuple, and tag take a closure that will add their fields to the FutureEncoder state.
    record : U64, (opaque, NamedFieldFn opaque val -> opaque) -> FutureEncoder state where val implements FutureEncoding, state implements FutureEncoderFormatting
    tuple : U64, (opaque, FieldFn opaque val -> opaque) -> FutureEncoder state where state implements FutureEncoderFormatting

    # TODO: We probably want an indexed tag as well.
    # It would take an integer index that maps to the tag variant.
    # Due to how roc tag unions work, it would only be valid for opaque tags.
    # That said, we could probably make it derive for opaque tags.
    tag : Str, U64, (opaque, FieldFn opaque val -> opaque) -> FutureEncoder state where state implements FutureEncoderFormatting

## Creates a custom encoder from a given function.
##
## ```roc
## expect
##     # Appends the byte 42
##     customFutureEncoder = FutureEncode.custom (\@Json bytes -> List.append bytes 42 |> @Json)
##
##     @Json actual = FutureEncode.appendWith (@Json []) customFutureEncoder
##     expected = [42] # Expected result is a list with a single byte, 42
##
##     actual == expected
## ```
custom : (state -> state) -> FutureEncoder state where state implements FutureEncoderFormatting
custom = \encoder -> @FutureEncoder encoder

appendWith : state, FutureEncoder state -> state where state implements FutureEncoderFormatting
appendWith = \state, @FutureEncoder doFutureEncoding -> doFutureEncoding state

## Appends the encoded representation of a value to an existing list of bytes.
##
## ```roc
## expect
##     @Json actual = FutureEncode.append (@Json []) { foo: 43 }
##     expected = Str.toUtf8 """{"foo":43}"""
##
##     actual == expected
## ```
append : state, val -> state where val implements FutureEncoding, state implements FutureEncoderFormatting
append = \state, val -> appendWith state (toFutureEncoder val)
