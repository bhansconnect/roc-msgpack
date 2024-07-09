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
    encode,
]

import FutureEncode exposing [FutureEncoder, FutureEncoderFormatting, SequenceWalker, MappingWalker, FutureEncoding]
import FutureDecode exposing [FutureDecoder, FutureDecoderFormatting, SequenceInit, SequenceBuilder, MappingInit, MappingBuilder, FutureDecoding]
# import FutureDecode

MsgPack := { bytes : List U8 }
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

encodeU8 : U8 -> FutureEncoder MsgPack

encodeU16 : U16 -> FutureEncoder MsgPack

encodeU32 : U32 -> FutureEncoder MsgPack

encodeU64 : U64 -> FutureEncoder MsgPack

encodeU128 : U128 -> FutureEncoder MsgPack

encodeI8 : I8 -> FutureEncoder MsgPack

encodeI16 : I16 -> FutureEncoder MsgPack

encodeI32 : I32 -> FutureEncoder MsgPack

encodeI64 : I64 -> FutureEncoder MsgPack

encodeI128 : I128 -> FutureEncoder MsgPack

encodeF32 : F32 -> FutureEncoder MsgPack

encodeF64 : F64 -> FutureEncoder MsgPack

encodeDec : Dec -> FutureEncoder MsgPack

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

encode : val -> List U8 where val implements FutureEncoding
encode = \val ->
    (@MsgPack { bytes }) = FutureEncode.append (@MsgPack { bytes: [] }) val
    bytes

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

