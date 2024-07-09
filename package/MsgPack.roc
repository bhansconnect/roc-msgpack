## MessagePack is an efficient binary serialization format.
## It lets you exchange data among multiple languages like JSON.
## But it's faster and smaller.
## Small integers are encoded into a single byte,
## and typical short strings require only one extra byte in addition to the strings themselves.
##
## This module implements functionality to serialise and de-serialise Roc types
## to and from MessagePack data. Using the `Encode` and `Decode` builtins this process
## can be achieved without the need to write custom encoder and decoder functions
## to parse UTF-8 strings.
##
## WARNING: Today, it uses FutureEncode and FutureDecode which are not yet implemented in core roc.
##
module [
    MsgPack,
    EncodeError,
    encode,
]

import FutureEncode exposing [FutureEncoder, FutureEncoderFormatting, SequenceWalker, MappingWalker, FutureEncoding]
import FutureDecode exposing [FutureDecoder, FutureDecoderFormatting, SequenceInit, SequenceBuilder, MappingInit, MappingBuilder, FutureDecoding]

EncodeError : [U128Unsupported, I128Unsupported, DecUnsupported]
EncodeState : List U8

MsgPack := Result EncodeState EncodeError
    implements [
        FutureEncoderFormatting {
            u8: encodeU8,
            u16: encodeU16,
            u32: encodeU32,
            u64: encodeU64,
            u128: encodeU128,
            i8: encodeI8,
            i16: encodeI16,
            i32: encodeI32,
            i64: encodeI64,
            i128: encodeI128,
            f32: encodeF32,
            f64: encodeF64,
            dec: encodeDec,
            bool: encodeBool,
            string: encodeString,
            sequence: encodeSequence,
            mapping: encodeMapping,
            record: encodeRecord,
            namedField: encodeNamedField,
            tuple: encodeTuple,
            tag: encodeTag,
            field: encodeField,
        },
        FutureDecoderFormatting {
            u8: decodeU8,
            u16: decodeU16,
            u32: decodeU32,
            u64: decodeU64,
            u128: decodeU128,
            i8: decodeI8,
            i16: decodeI16,
            i32: decodeI32,
            i64: decodeI64,
            i128: decodeI128,
            f32: decodeF32,
            f64: decodeF64,
            dec: decodeDec,
            bool: decodeBool,
            string: decodeString,
            sequence: decodeSequence,
            mapping: decodeMapping,
            record: decodeRecord,
            tuple: decodeTuple,
        },
    ]

# =====================================
# Encode
# =====================================

# TODO: When the compiler is fixed, remove all the Num.intCast's for this constant.
maxPosFixInt = 0x7F
minNegFixInt = -33 # 0xDF

encodeU8 : U8 -> FutureEncoder MsgPack
encodeU8 = \n ->
    if n <= (Num.intCast maxPosFixInt) then
        encodePosFixInt n
    else
        encodeUInt8 n

expect
    got = encode (@TestU8 0x07)
    want = Ok [0x07]
    got == want

expect
    got = encode (@TestU8 0xF8)
    want = Ok [0xCC, 0xF8]
    got == want

encodeU16 : U16 -> FutureEncoder MsgPack
encodeU16 = \n ->
    if n <= (Num.intCast maxPosFixInt) then
        encodePosFixInt (Num.toU8 n)
    else if n <= 0xFF then
        encodeUInt8 (Num.toU8 n)
    else
        encodeUInt16 n

expect
    got = encode (@TestU16 0x07)
    want = Ok [0x07]
    got == want

expect
    got = encode (@TestU16 0xF8)
    want = Ok [0xCC, 0xF8]
    got == want

expect
    got = encode (@TestU16 0x02BC)
    want = Ok [0xCD, 0x02, 0xBC]
    got == want

encodeU32 : U32 -> FutureEncoder MsgPack
encodeU32 = \n ->
    if n <= (Num.intCast maxPosFixInt) then
        encodePosFixInt (Num.toU8 n)
    else if n <= 0xFF then
        encodeUInt8 (Num.toU8 n)
    else if n <= 0xFFFF then
        encodeUInt16 (Num.toU16 n)
    else
        encodeUInt32 n

expect
    got = encode (@TestU32 0x07)
    want = Ok [0x07]
    got == want

expect
    got = encode (@TestU32 0xF8)
    want = Ok [0xCC, 0xF8]
    got == want

expect
    got = encode (@TestU32 0x02BC)
    want = Ok [0xCD, 0x02, 0xBC]
    got == want

expect
    got = encode (@TestU32 0xDEAD_BEEF)
    want = Ok [0xCE, 0xDE, 0xAD, 0xBE, 0xEF]
    got == want

encodeU64 : U64 -> FutureEncoder MsgPack
encodeU64 = \n ->
    if n <= (Num.intCast maxPosFixInt) then
        encodePosFixInt (Num.toU8 n)
    else if n <= 0xFF then
        encodeUInt8 (Num.toU8 n)
    else if n <= 0xFFFF then
        encodeUInt16 (Num.toU16 n)
    else if n <= 0xFFFF_FFFF then
        encodeUInt32 (Num.toU32 n)
    else
        encodeUInt64 n

expect
    got = encode (@TestU64 0x07)
    want = Ok [0x07]
    got == want

expect
    got = encode (@TestU64 0xF8)
    want = Ok [0xCC, 0xF8]
    got == want

expect
    got = encode (@TestU64 0x02BC)
    want = Ok [0xCD, 0x02, 0xBC]
    got == want

expect
    got = encode (@TestU64 0xDEAD_BEEF)
    want = Ok [0xCE, 0xDE, 0xAD, 0xBE, 0xEF]
    got == want

expect
    got = encode (@TestU64 0x1507_DEAD_BEEF)
    want = Ok [0xCF, 0x00, 0x00, 0x15, 0x07, 0xDE, 0xAD, 0xBE, 0xEF]
    got == want

encodeU128 : U128 -> FutureEncoder MsgPack
encodeU128 = \_ ->
    _ <- tryEncode
    Err U128Unsupported

expect
    got = encode (@TestU128 0xBAD)
    want = Err U128Unsupported
    got == want

encodeI8 : I8 -> FutureEncoder MsgPack
encodeI8 = \n ->
    if n >= 0 then
        encodeU8 (Num.toU8 n)
    else if n > (Num.intCast minNegFixInt) then
        encodeNegFixInt n
    else
        encodeInt8 n

expect
    got = encode (@TestI8 7)
    want = Ok [0x07]
    got == want

expect
    got = encode (@TestI8 -7)
    want = Ok [0xF9]
    got == want

expect
    got = encode (@TestI8 -33)
    want = Ok [0xD0, 0xDF]
    got == want

encodeI16 : I16 -> FutureEncoder MsgPack
encodeI16 = \n ->
    if n >= 0 then
        encodeU16 (Num.toU16 n)
    else if n > (Num.intCast minNegFixInt) then
        encodeNegFixInt (Num.toI8 n)
    else if n > 0xFF80 then
        encodeInt8 (Num.toI8 n)
    else
        encodeInt16 n

expect
    got = encode (@TestI16 255)
    want = Ok [0xCC, 0xFF]
    got == want

expect
    got = encode (@TestI16 -7)
    want = Ok [0xF9]
    got == want

expect
    got = encode (@TestI16 -33)
    want = Ok [0xD0, 0xDF]
    got == want

expect
    got = encode (@TestI16 -129)
    want = Ok [0xD1, 0xFF, 0x7F]
    got == want

encodeI32 : I32 -> FutureEncoder MsgPack
encodeI32 = \n ->
    if n >= 0 then
        encodeU32 (Num.toU32 n)
    else if n > (Num.intCast minNegFixInt) then
        encodeNegFixInt (Num.toI8 n)
    else if n > 0xFFFF_FF80 then
        encodeInt8 (Num.toI8 n)
    else if n > 0xFFFF_8000 then
        encodeInt16 (Num.toI16 n)
    else
        encodeInt32 n

expect
    got = encode (@TestI32 65535)
    want = Ok [0xCD, 0xFF, 0xFF]
    got == want

expect
    got = encode (@TestI32 -7)
    want = Ok [0xF9]
    got == want

expect
    got = encode (@TestI32 -33)
    want = Ok [0xD0, 0xDF]
    got == want

expect
    got = encode (@TestI32 -129)
    want = Ok [0xD1, 0xFF, 0x7F]
    got == want

expect
    got = encode (@TestI32 -32768)
    want = Ok [0xD2, 0xFF, 0xFF, 0x80, 0x00]
    got == want

encodeI64 : I64 -> FutureEncoder MsgPack
encodeI64 = \n ->
    if n >= 0 then
        encodeU64 (Num.toU64 n)
    else if n > (Num.intCast minNegFixInt) then
        encodeNegFixInt (Num.toI8 n)
    else if n > 0xFFFF_FFFF_FFFF_FF80 then
        encodeInt8 (Num.toI8 n)
    else if n > 0xFFFF_FFFF_FFFF_8000 then
        encodeInt16 (Num.toI16 n)
    else if n > 0xFFFF_FFFF_8000_0000 then
        encodeInt32 (Num.toI32 n)
    else
        encodeInt64 n

expect
    got = encode (@TestI64 4294967295)
    want = Ok [0xCE, 0xFF, 0xFF, 0xFF, 0xFF]
    got == want

expect
    got = encode (@TestI64 -7)
    want = Ok [0xF9]
    got == want

expect
    got = encode (@TestI64 -33)
    want = Ok [0xD0, 0xDF]
    got == want

expect
    got = encode (@TestI64 -32768)
    want = Ok [0xD2, 0xFF, 0xFF, 0x80, 0x00]
    got == want

expect
    got = encode (@TestI64 -129)
    want = Ok [0xD1, 0xFF, 0x7F]
    got == want

expect
    got = encode (@TestI64 -2147483648)
    want = Ok [0xD3, 0xFF, 0xFF, 0xFF, 0xFF, 0x80, 0x00, 0x00, 0x00]
    got == want

encodeI128 : I128 -> FutureEncoder MsgPack
encodeI128 = \_ ->
    _ <- tryEncode
    Err I128Unsupported

expect
    got = encode (@TestI128 0xBAD)
    want = Err I128Unsupported
    got == want

encodeF32 : F32 -> FutureEncoder MsgPack

encodeF64 : F64 -> FutureEncoder MsgPack

encodeDec : Dec -> FutureEncoder MsgPack
encodeDec = \_ ->
    _ <- tryEncode
    Err DecUnsupported

expect
    got = encode (@TestDec 0.2)
    want = Err DecUnsupported
    got == want

encodeBool : Bool -> FutureEncoder MsgPack

encodeString : Str -> FutureEncoder MsgPack

encodeSequence :
    seq,
    [Size U64, UnknownSize],
    SequenceWalker MsgPack seq elem,
    (elem -> FutureEncoder MsgPack)
    -> FutureEncoder MsgPack

encodeMapping :
    map,
    [Size U64, UnknownSize],
    MappingWalker MsgPack key val elem,
    (key -> FutureEncoder MsgPack),
    (val -> FutureEncoder MsgPack)
    -> FutureEncoder MsgPack

encodeRecord : U64, (FutureEncoder MsgPack -> FutureEncoder MsgPack) -> FutureEncoder MsgPack

encodeNamedField : FutureEncoder MsgPack, { key : Str, value : FutureEncoder MsgPack } -> FutureEncoder MsgPack

encodeTuple : U64, (FutureEncoder MsgPack -> FutureEncoder MsgPack) -> FutureEncoder MsgPack

encodeTag : Str, U64, (FutureEncoder MsgPack -> FutureEncoder MsgPack) -> FutureEncoder MsgPack

encodeField : FutureEncoder MsgPack, FutureEncoder MsgPack -> FutureEncoder MsgPack

encode : val -> Result (List U8) EncodeError where val implements FutureEncoding
encode = \val ->
    (@MsgPack res) = FutureEncode.append (@MsgPack (Ok [])) val
    res

# =====================================
# Exact MsgPack Type Encoders
# =====================================
# These are for the exact types in the msgpack spec
# These assume the correct encoder was chosen that will lead to minimal sized results.

tryEncode : (EncodeState -> Result EncodeState EncodeError) -> FutureEncoder MsgPack
tryEncode = \cont ->
    FutureEncode.custom \@MsgPack res ->
        Result.try res cont
        |> @MsgPack

encodePosFixInt : U8 -> FutureEncoder MsgPack
encodePosFixInt = \n ->
    bytes <- tryEncode
    bytes
    |> List.append n
    |> Ok

encodeUInt8 : U8 -> FutureEncoder MsgPack
encodeUInt8 = \n ->
    bytes <- tryEncode
    bytes
    |> List.reserve 2
    |> List.append 0xCC
    |> List.append n
    |> Ok

encodeUInt16 : U16 -> FutureEncoder MsgPack
encodeUInt16 = \n ->
    bytes <- tryEncode
    b0 = Num.shiftRightZfBy n 8 |> Num.toU8
    b1 = Num.toU8 n
    bytes
    |> List.reserve 3
    |> List.append 0xCD
    |> List.append b0
    |> List.append b1
    |> Ok

encodeUInt32 : U32 -> FutureEncoder MsgPack
encodeUInt32 = \n ->
    bytes <- tryEncode
    b0 = Num.shiftRightZfBy n 24 |> Num.toU8
    b1 = Num.shiftRightZfBy n 16 |> Num.toU8
    b2 = Num.shiftRightZfBy n 8 |> Num.toU8
    b3 = Num.toU8 n
    bytes
    |> List.reserve 5
    |> List.append 0xCE
    |> List.append b0
    |> List.append b1
    |> List.append b2
    |> List.append b3
    |> Ok

encodeUInt64 : U64 -> FutureEncoder MsgPack
encodeUInt64 = \n ->
    bytes <- tryEncode
    b0 = Num.shiftRightZfBy n 56 |> Num.toU8
    b1 = Num.shiftRightZfBy n 48 |> Num.toU8
    b2 = Num.shiftRightZfBy n 40 |> Num.toU8
    b3 = Num.shiftRightZfBy n 32 |> Num.toU8
    b4 = Num.shiftRightZfBy n 24 |> Num.toU8
    b5 = Num.shiftRightZfBy n 16 |> Num.toU8
    b6 = Num.shiftRightZfBy n 8 |> Num.toU8
    b7 = Num.toU8 n
    bytes
    |> List.reserve 9
    |> List.append 0xCF
    |> List.append b0
    |> List.append b1
    |> List.append b2
    |> List.append b3
    |> List.append b4
    |> List.append b5
    |> List.append b6
    |> List.append b7
    |> Ok

encodeNegFixInt : I8 -> FutureEncoder MsgPack
encodeNegFixInt = \in ->
    bytes <- tryEncode
    bytes
    |> List.append (Num.toU8 in)
    |> Ok

encodeInt8 : I8 -> FutureEncoder MsgPack
encodeInt8 = \in ->
    bytes <- tryEncode
    bytes
    |> List.reserve 2
    |> List.append 0xD0
    |> List.append (Num.toU8 in)
    |> Ok

encodeInt16 : I16 -> FutureEncoder MsgPack
encodeInt16 = \in ->
    bytes <- tryEncode
    n = Num.toU16 in
    b0 = Num.shiftRightZfBy n 8 |> Num.toU8
    b1 = Num.toU8 n
    bytes
    |> List.reserve 3
    |> List.append 0xD1
    |> List.append b0
    |> List.append b1
    |> Ok

encodeInt32 : I32 -> FutureEncoder MsgPack
encodeInt32 = \in ->
    bytes <- tryEncode
    n = Num.toU32 in
    b0 = Num.shiftRightZfBy n 24 |> Num.toU8
    b1 = Num.shiftRightZfBy n 16 |> Num.toU8
    b2 = Num.shiftRightZfBy n 8 |> Num.toU8
    b3 = Num.toU8 n
    bytes
    |> List.reserve 5
    |> List.append 0xD2
    |> List.append b0
    |> List.append b1
    |> List.append b2
    |> List.append b3
    |> Ok

encodeInt64 : I64 -> FutureEncoder MsgPack
encodeInt64 = \in ->
    bytes <- tryEncode
    n = Num.toU64 in
    b0 = Num.shiftRightZfBy n 56 |> Num.toU8
    b1 = Num.shiftRightZfBy n 48 |> Num.toU8
    b2 = Num.shiftRightZfBy n 40 |> Num.toU8
    b3 = Num.shiftRightZfBy n 32 |> Num.toU8
    b4 = Num.shiftRightZfBy n 24 |> Num.toU8
    b5 = Num.shiftRightZfBy n 16 |> Num.toU8
    b6 = Num.shiftRightZfBy n 8 |> Num.toU8
    b7 = Num.toU8 n
    bytes
    |> List.reserve 9
    |> List.append 0xD3
    |> List.append b0
    |> List.append b1
    |> List.append b2
    |> List.append b3
    |> List.append b4
    |> List.append b5
    |> List.append b6
    |> List.append b7
    |> Ok

# =====================================
# Decode
# =====================================

decodeU8 : FutureDecoder MsgPack U8 err

decodeU16 : FutureDecoder MsgPack U16 err

decodeU32 : FutureDecoder MsgPack U32 err

decodeU64 : FutureDecoder MsgPack U64 err

decodeU128 : FutureDecoder MsgPack U128 err

decodeI8 : FutureDecoder MsgPack I8 err

decodeI16 : FutureDecoder MsgPack I16 err

decodeI32 : FutureDecoder MsgPack I32 err

decodeI64 : FutureDecoder MsgPack I64 err

decodeI128 : FutureDecoder MsgPack I128 err

decodeF32 : FutureDecoder MsgPack F32 err

decodeF64 : FutureDecoder MsgPack F64 err

decodeDec : FutureDecoder MsgPack Dec err

decodeBool : FutureDecoder MsgPack Bool err

decodeString : FutureDecoder MsgPack Str err

decodeSequence :
    SequenceInit seq,
    SequenceBuilder seq elem,
    FutureDecoder MsgPack elem err
    -> FutureDecoder MsgPack seq err

decodeMapping :
    MappingInit map,
    MappingBuilder map key val,
    FutureDecoder MsgPack key err,
    FutureDecoder MsgPack val err
    -> FutureDecoder MsgPack map err

decodeRecord :
    MsgPack,
    List Str,
    (MsgPack, U64 -> [Next (FutureDecoder MsgPack MsgPack err), TooLong]),
    (MsgPack -> Result val err)
    -> FutureDecoder MsgPack val err

decodeTuple :
    MsgPack,
    (MsgPack, U64 -> [Next (FutureDecoder MsgPack MsgPack err), TooLong]),
    (MsgPack -> Result val err)
    -> FutureDecoder MsgPack val err

# =====================================
# Types for Testing
# =====================================
# These would auto-derived once implemented in the real compiler.

TestU8 := U8
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestU8,
        },
        FutureDecoding {
            decoder: decoderTestU8,
        },
    ]

toFutureEncoderTestU8 : TestU8 -> FutureEncoder state
toFutureEncoderTestU8 = \@TestU8 u8 ->
    FutureEncode.u8 u8

decoderTestU8 : FutureDecoder state TestU8 err where state implements FutureDecoderFormatting
decoderTestU8 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.u8
    |> FutureDecode.mapResult @TestU8

TestU16 := U16
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestU16,
        },
        FutureDecoding {
            decoder: decoderTestU16,
        },
    ]

toFutureEncoderTestU16 : TestU16 -> FutureEncoder state
toFutureEncoderTestU16 = \@TestU16 u16 ->
    FutureEncode.u16 u16

decoderTestU16 : FutureDecoder state TestU16 err where state implements FutureDecoderFormatting
decoderTestU16 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.u16
    |> FutureDecode.mapResult @TestU16

TestU32 := U32
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestU32,
        },
        FutureDecoding {
            decoder: decoderTestU32,
        },
    ]

toFutureEncoderTestU32 : TestU32 -> FutureEncoder state
toFutureEncoderTestU32 = \@TestU32 u32 ->
    FutureEncode.u32 u32

decoderTestU32 : FutureDecoder state TestU32 err where state implements FutureDecoderFormatting
decoderTestU32 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.u32
    |> FutureDecode.mapResult @TestU32

TestU64 := U64
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestU64,
        },
        FutureDecoding {
            decoder: decoderTestU64,
        },
    ]

toFutureEncoderTestU64 : TestU64 -> FutureEncoder state
toFutureEncoderTestU64 = \@TestU64 u64 ->
    FutureEncode.u64 u64

decoderTestU64 : FutureDecoder state TestU64 err where state implements FutureDecoderFormatting
decoderTestU64 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.u64
    |> FutureDecode.mapResult @TestU64

TestU128 := U128
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestU128,
        },
        FutureDecoding {
            decoder: decoderTestU128,
        },
    ]

toFutureEncoderTestU128 : TestU128 -> FutureEncoder state
toFutureEncoderTestU128 = \@TestU128 u128 ->
    FutureEncode.u128 u128

decoderTestU128 : FutureDecoder state TestU128 err where state implements FutureDecoderFormatting
decoderTestU128 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.u128
    |> FutureDecode.mapResult @TestU128

TestI8 := I8
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestI8,
        },
        FutureDecoding {
            decoder: decoderTestI8,
        },
    ]

toFutureEncoderTestI8 : TestI8 -> FutureEncoder state
toFutureEncoderTestI8 = \@TestI8 i8 ->
    FutureEncode.i8 i8

decoderTestI8 : FutureDecoder state TestI8 err where state implements FutureDecoderFormatting
decoderTestI8 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.i8
    |> FutureDecode.mapResult @TestI8

TestI16 := I16
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestI16,
        },
        FutureDecoding {
            decoder: decoderTestI16,
        },
    ]

toFutureEncoderTestI16 : TestI16 -> FutureEncoder state
toFutureEncoderTestI16 = \@TestI16 i16 ->
    FutureEncode.i16 i16

decoderTestI16 : FutureDecoder state TestI16 err where state implements FutureDecoderFormatting
decoderTestI16 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.i16
    |> FutureDecode.mapResult @TestI16

TestI32 := I32
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestI32,
        },
        FutureDecoding {
            decoder: decoderTestI32,
        },
    ]

toFutureEncoderTestI32 : TestI32 -> FutureEncoder state
toFutureEncoderTestI32 = \@TestI32 i32 ->
    FutureEncode.i32 i32

decoderTestI32 : FutureDecoder state TestI32 err where state implements FutureDecoderFormatting
decoderTestI32 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.i32
    |> FutureDecode.mapResult @TestI32

TestI64 := I64
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestI64,
        },
        FutureDecoding {
            decoder: decoderTestI64,
        },
    ]

toFutureEncoderTestI64 : TestI64 -> FutureEncoder state
toFutureEncoderTestI64 = \@TestI64 i64 ->
    FutureEncode.i64 i64

decoderTestI64 : FutureDecoder state TestI64 err where state implements FutureDecoderFormatting
decoderTestI64 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.i64
    |> FutureDecode.mapResult @TestI64

TestI128 := I128
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestI128,
        },
        FutureDecoding {
            decoder: decoderTestI128,
        },
    ]

toFutureEncoderTestI128 : TestI128 -> FutureEncoder state
toFutureEncoderTestI128 = \@TestI128 i128 ->
    FutureEncode.i128 i128

decoderTestI128 : FutureDecoder state TestI128 err where state implements FutureDecoderFormatting
decoderTestI128 = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.i128
    |> FutureDecode.mapResult @TestI128

TestDec := Dec
    implements [
        FutureEncoding {
            toFutureEncoder: toFutureEncoderTestDec,
        },
        FutureDecoding {
            decoder: decoderTestDec,
        },
    ]

toFutureEncoderTestDec : TestDec -> FutureEncoder state
toFutureEncoderTestDec = \@TestDec dec ->
    FutureEncode.dec dec

decoderTestDec : FutureDecoder state TestDec err where state implements FutureDecoderFormatting
decoderTestDec = FutureDecode.custom \state ->
    FutureDecode.decodeWith state FutureDecode.dec
    |> FutureDecode.mapResult @TestDec

