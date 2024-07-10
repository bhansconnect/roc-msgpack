module [
    FutureDecodeResult,
    FutureDecoder,
    FutureDecoding,
    FutureDecoderFormatting,
    decoder,
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
    tuple,
    custom,
    SequenceInit,
    SequenceBuilder,
    MappingInit,
    MappingBuilder,
    decode,
    decodeWith,
    mapResult,
    LengthInfo,
]

## Return type of a [FutureDecoder].
FutureDecodeResult state val err : Result (state, val) err

## Decodes from a `state` which implements [FutureDecoderFormatting]
## to a `val` that is the type of the decoded value
FutureDecoder state val err := state -> FutureDecodeResult state val err where state implements FutureDecoderFormatting

## Definition of the [FutureDecoding] ability
FutureDecoding implements
    decoder : FutureDecoder state val err where val implements FutureDecoding, state implements FutureDecoderFormatting

SequenceInit seq : LengthInfo -> seq
SequenceBuilder seq elem : seq, elem -> seq

MappingInit map : LengthInfo -> map
MappingBuilder map key val : map, key, val -> map

LengthInfo : [Length U64, UnknownLength]

## Definition of the [FutureDecoderFormatting] ability
FutureDecoderFormatting implements
    u8 : FutureDecoder state U8 err where state implements FutureDecoderFormatting
    u16 : FutureDecoder state U16 err where state implements FutureDecoderFormatting
    u32 : FutureDecoder state U32 err where state implements FutureDecoderFormatting
    u64 : FutureDecoder state U64 err where state implements FutureDecoderFormatting
    u128 : FutureDecoder state U128 err where state implements FutureDecoderFormatting
    i8 : FutureDecoder state I8 err where state implements FutureDecoderFormatting
    i16 : FutureDecoder state I16 err where state implements FutureDecoderFormatting
    i32 : FutureDecoder state I32 err where state implements FutureDecoderFormatting
    i64 : FutureDecoder state I64 err where state implements FutureDecoderFormatting
    i128 : FutureDecoder state I128 err where state implements FutureDecoderFormatting
    f32 : FutureDecoder state F32 err where state implements FutureDecoderFormatting
    f64 : FutureDecoder state F64 err where state implements FutureDecoderFormatting
    dec : FutureDecoder state Dec err where state implements FutureDecoderFormatting
    bool : FutureDecoder state Bool err where state implements FutureDecoderFormatting
    string : FutureDecoder state Str err where state implements FutureDecoderFormatting
    sequence :
        SequenceInit seq,
        SequenceBuilder seq elem,
        FutureDecoder state elem err
        -> FutureDecoder state seq err where state implements FutureDecoderFormatting
    mapping :
        MappingInit map,
        MappingBuilder map key val,
        FutureDecoder state key err,
        FutureDecoder state val err
        -> FutureDecoder state map err where state implements FutureDecoderFormatting

    ## `record state stepField finalizer` decodes a record field-by-field.
    ##
    ## `stepField` returns a decoder for the given field in the record, or
    ## `Skip` if the field is not a part of the decoded record.
    ##
    ## `finalizer` should produce the record value from the decoded `state`.
    record :
        state,
        List Str,
        (state, U64 -> [Next (FutureDecoder state state err), TooLong]),
        (state -> Result val err)
        -> FutureDecoder state val err where state implements FutureDecoderFormatting

    ## `tuple state stepElem finalizer` decodes a tuple element-by-element.
    ##
    ## `stepElem` returns a decoder for the nth index in the tuple, or
    ## `TooLong` if the index is larger than the expected size of the tuple. The
    ## index passed to `stepElem` is 0-indexed.
    ##
    ## `finalizer` should produce the tuple value from the decoded `state`.
    tuple :
        state,
        (state, U64 -> [Next (FutureDecoder state state err), TooLong]),
        (state -> Result val err)
        -> FutureDecoder state val err where state implements FutureDecoderFormatting

## Build a custom [FutureDecoder] function. For example the implementation of
## `decodeBool` could be defined as follows;
##
## ```roc
## decodeBool = FutureDecode.custom \@Json bytes ->
##     when bytes is
##         ['f', 'a', 'l', 's', 'e', .. as rest] -> Ok (@Json rest, Bool.false)
##         ['t', 'r', 'u', 'e', .. as rest] -> {Ok (@Json rest, Bool.true)
##         _ -> Err TooShort
## ```
custom : (state -> FutureDecodeResult state val err) -> FutureDecoder state val err where state implements FutureDecoderFormatting
custom = \decoderFn -> @FutureDecoder decoderFn

decode : state -> FutureDecodeResult state val err where val implements FutureDecoding, state implements FutureDecoderFormatting
decode = \state -> decodeWith state decoder

## Decode a state using a specific [FutureDecoder] function
decodeWith : state, FutureDecoder state val err -> FutureDecodeResult state val err where state implements FutureDecoderFormatting
decodeWith = \state, @FutureDecoder decodeFn -> decodeFn state

## Transform the `val` of a [FutureDecodeResult]
mapResult : FutureDecodeResult state a err, (a -> b) -> FutureDecodeResult state b err
mapResult = \result, mapper -> Result.map result \(state, a) -> (state, mapper a)
