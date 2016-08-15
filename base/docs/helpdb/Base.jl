# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base

"""
    systemerror(sysfunc, iftrue)

Raises a `SystemError` for `errno` with the descriptive string `sysfunc` if `iftrue` is `true`
"""
systemerror

"""
    fill!(A, x)

Fill array `A` with the value `x`. If `x` is an object reference, all elements will refer to
the same object. `fill!(A, Foo())` will return `A` filled with the result of evaluating
`Foo()` once.
"""
fill!

"""
    read!(stream::IO, array::Union{Array, BitArray})
    read!(filename::AbstractString, array::Union{Array, BitArray})

Read binary data from an I/O stream or file, filling in `array`.
"""
read!

"""
    empty!(collection) -> collection

Remove all elements from a `collection`.
"""
empty!

"""
    asin(x)

Compute the inverse sine of `x`, where the output is in radians.
"""
asin

"""
    takebuf_array(b::IOBuffer)

Obtain the contents of an `IOBuffer` as an array, without copying. Afterwards, the
`IOBuffer` is reset to its initial state.
"""
takebuf_array

"""
    download(url,[localfile])

Download a file from the given url, optionally renaming it to the given local file name.
Note that this function relies on the availability of external tools such as `curl`, `wget`
or `fetch` to download the file and is provided for convenience. For production use or
situations in which more options are needed, please use a package that provides the desired
functionality instead.
"""
download

"""
    lstrip(string, [chars])

Return `string` with any leading whitespace removed. If `chars` (a character, or vector or
set of characters) is provided, instead remove characters contained in it.
"""
lstrip

"""
    pointer(array [, index])

Get the native address of an array or string element. Be careful to ensure that a Julia
reference to `a` exists as long as this pointer will be used. This function is "unsafe" like
`unsafe_convert`.

Calling `Ref(array[, index])` is generally preferable to this function.
"""
pointer

"""
    println(x)

Print (using [`print`](:func:`print`)) `x` followed by a newline.
"""
println

"""
    //(num, den)

Divide two integers or rational numbers, giving a `Rational` result.
"""
Base.:(//)

"""
    isinteger(x) -> Bool

Test whether `x` or all its elements are numerically equal to some integer
"""
isinteger

"""
    ./(x, y)

Element-wise right division operator.
"""
Base.:(./)

"""
    IPv6(host::Integer) -> IPv6

Returns IPv6 object from ip address formatted as Integer
"""
IPv6

"""
    prod!(r, A)

Multiply elements of `A` over the singleton dimensions of `r`, and write results to `r`.
"""
prod!

"""
    cummin(A, [dim])

Cumulative minimum along a dimension. The dimension defaults to 1.
"""
cummin

"""
    minabs!(r, A)

Compute the minimum absolute values over the singleton dimensions of `r`, and write values to `r`.
"""
minabs!

"""
    eigfact!(A, [B])

Same as [`eigfact`](:func:`eigfact`), but saves space by overwriting the input `A` (and
`B`), instead of creating a copy.
"""
eigfact!

"""
    cosh(x)

Compute hyperbolic cosine of `x`.
"""
cosh

"""
    dirname(path::AbstractString) -> AbstractString

Get the directory part of a path.
"""
dirname

"""
    isfile(path) -> Bool

Returns `true` if `path` is a regular file, `false` otherwise.
"""
isfile

"""
    precision(num::AbstractFloat)

Get the precision of a floating point number, as defined by the effective number of bits in
the mantissa.
"""
precision

"""
    readlines(stream::IO)
    readlines(filename::AbstractString)

Read all lines of an I/O stream or a file as a vector of strings.
The text is assumed to be encoded in UTF-8.
"""
readlines

"""
    promote_type(type1, type2)

Determine a type big enough to hold values of each argument type without loss, whenever
possible. In some cases, where no type exists to which both types can be promoted
losslessly, some loss is tolerated; for example, `promote_type(Int64,Float64)` returns
`Float64` even though strictly, not all `Int64` values can be represented exactly as
`Float64` values.
"""
promote_type

"""
```
.*(x, y)
```

Element-wise multiplication operator.
"""
Base.:(.*)

"""
    edit(path::AbstractString, [line])

Edit a file or directory optionally providing a line number to edit the file at. Returns to
the `julia` prompt when you quit the editor.
"""
edit(path::AbstractString, line=?)

"""
    edit(function, [types])

Edit the definition of a function, optionally specifying a tuple of types to indicate which
method to edit.
"""
edit(::Function, types=?)

"""
    backtrace()

Get a backtrace object for the current program point.
"""
backtrace

"""
    -(x)

Unary minus operator.
"""
-(x)

"""
    -(x, y)

Subtraction operator.
"""
-(x, y)

"""
    Nullable(x)

Wrap value `x` in an object of type `Nullable`, which indicates whether a value is present.
`Nullable(x)` yields a non-empty wrapper, and `Nullable{T}()` yields an empty instance of a
wrapper that might contain a value of type `T`.
"""
Nullable

"""
    bits(n)

A string giving the literal bit representation of a number.
"""
bits

"""
    getindex(type[, elements...])

Construct a 1-d array of the specified type. This is usually called with the syntax
`Type[]`. Element values can be specified using `Type[a,b,c,...]`.
"""
getindex(::Type, elements...)

"""
    getindex(A, inds...)

Returns a subset of array `A` as specified by `inds`, where each `ind` may be an
`Int`, a `Range`, or a `Vector`. See the manual section on
[array indexing](:ref:`array indexing <man-array-indexing>`) for details.
"""
getindex(::AbstractArray, inds...)

"""
    getindex(collection, key...)

Retrieve the value(s) stored at the given key or index within a collection. The syntax
`a[i,j,...]` is converted by the compiler to `getindex(a, i, j, ...)`.
"""
getindex(collection, key...)

"""
    cconvert(T,x)

Convert `x` to a value of type `T`, typically by calling `convert(T,x)`

In cases where `x` cannot be safely converted to `T`, unlike `convert`, `cconvert` may
return an object of a type different from `T`, which however is suitable for
`unsafe_convert` to handle.

Neither `convert` nor `cconvert` should take a Julia object and turn it into a `Ptr`.
"""
cconvert

"""
    assert(cond)

Throw an `AssertionError` if `cond` is `false`. Also available as the macro `@assert expr`.
"""
assert

"""
    sech(x)

Compute the hyperbolic secant of `x`
"""
sech

"""
    filemode(file)

Equivalent to `stat(file).mode`
"""
filemode

"""
    join(io, items, delim, [last])

Print elements of `items` to `io` with `delim` between them. If `last` is specified, it is
used as the final delimiter instead of `delim`.
"""
join(io, items, delim, last)

"""
    deconv(b,a)

Construct vector `c` such that `b = conv(a,c) + r`. Equivalent to polynomial division.
"""
deconv

"""
    acos(x)

Compute the inverse cosine of `x`, where the output is in radians
"""
acos

"""
    ispath(path) -> Bool

Returns `true` if `path` is a valid filesystem path, `false` otherwise.
"""
ispath

"""
    fdio([name::AbstractString, ]fd::Integer[, own::Bool]) -> IOStream

Create an `IOStream` object from an integer file descriptor. If `own` is `true`, closing
this object will close the underlying descriptor. By default, an `IOStream` is closed when
it is garbage collected. `name` allows you to associate the descriptor with a named file.
"""
fdio

"""
    unsafe_copy!(dest::Ptr{T}, src::Ptr{T}, N)

Copy `N` elements from a source pointer to a destination, with no checking. The size of an
element is determined by the type of the pointers.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointers `dest` and `src` to ensure that they are valid. Incorrect usage may corrupt or
segfault your program, in the same manner as C.
"""
unsafe_copy!{T}(dest::Ptr{T}, src::Ptr{T}, N)

"""
    unsafe_copy!(dest::Array, do, src::Array, so, N)

Copy `N` elements from a source array to a destination, starting at offset `so` in the
source and `do` in the destination (1-indexed).

The `unsafe` prefix on this function indicates that no validation is performed to ensure
that N is inbounds on either array. Incorrect usage may corrupt or segfault your program, in
the same manner as C.
"""
unsafe_copy!(dest::Array, d, src::Array, so, N)

"""
    .^(x, y)

Element-wise exponentiation operator.
"""
Base.:(.^)

"""
    isspace(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is any whitespace character. Includes ASCII characters '\\t',
'\\n', '\\v', '\\f', '\\r', and ' ', Latin-1 character U+0085, and characters in Unicode
category Zs. For strings, tests whether this is true for all elements of the string.
"""
isspace

"""
    splitext(path::AbstractString) -> (AbstractString,AbstractString)

If the last component of a path contains a dot, split the path into everything before the
dot and everything including and after the dot. Otherwise, return a tuple of the argument
unmodified and the empty string.
"""
splitext

"""
    gethostname() -> AbstractString

Get the local machine's host name.
"""
gethostname

"""
    replace(string, pat, r[, n])

Search for the given pattern `pat`, and replace each occurrence with `r`. If `n` is
provided, replace at most `n` occurrences. As with search, the second argument may be a
single character, a vector or a set of characters, a string, or a regular expression. If `r`
is a function, each occurrence is replaced with `r(s)` where `s` is the matched substring.
If `pat` is a regular expression and `r` is a `SubstitutionString`, then capture group
references in `r` are replaced with the corresponding matched text.
"""
replace

"""
    chop(string)

Remove the last character from a string.
"""
chop

"""
    Float32(x [, mode::RoundingMode])

Create a Float32 from `x`. If `x` is not exactly representable then `mode` determines how
`x` is rounded.

```jldoctest
julia> Float32(1/3, RoundDown)
0.3333333f0

julia> Float32(1/3, RoundUp)
0.33333334f0
```

See [`RoundingMode`](:obj:`RoundingMode`) for available rounding modes.
"""
Float32

"""
    readuntil(stream::IO, delim)
    readuntil(filename::AbstractString, delim)

Read a string from an I/O stream or a file, up to and including the given delimiter byte.
The text is assumed to be encoded in UTF-8.
"""
readuntil

"""
    issticky(path) -> Bool

Returns `true` if `path` has the sticky bit set, `false` otherwise.
"""
issticky

"""
    Mmap.mmap(io::Union{IOStream,AbstractString,Mmap.AnonymousMmap}[, type::Type{Array{T,N}}, dims, offset]; grow::Bool=true, shared::Bool=true)
           Mmap.mmap(type::Type{Array{T,N}}, dims)

Create an `Array` whose values are linked to a file, using memory-mapping. This provides a
convenient way of working with data too large to fit in the computer's memory.

The type is an `Array{T,N}` with a bits-type element of `T` and dimension `N` that
determines how the bytes of the array are interpreted. Note that the file must be stored in
binary format, and no format conversions are possible (this is a limitation of operating
systems, not Julia).

`dims` is a tuple or single `Integer` specifying the size or length of the array.

The file is passed via the stream argument, either as an open `IOStream` or filename string.
When you initialize the stream, use `"r"` for a "read-only" array, and `"w+"` to create a
new array used to write values to disk.

If no `type` argument is specified, the default is `Vector{UInt8}`.

Optionally, you can specify an offset (in bytes) if, for example, you want to skip over a
header in the file. The default value for the offset is the current stream position for an
`IOStream`.

The `grow` keyword argument specifies whether the disk file should be grown to accommodate
the requested size of array (if the total file size is < requested array size). Write
privileges are required to grow the file.

The `shared` keyword argument specifies whether the resulting `Array` and changes made to it
will be visible to other processes mapping the same file.

For example, the following code

```julia
# Create a file for mmapping
# (you could alternatively use mmap to do this step, too)
A = rand(1:20, 5, 30)
s = open("/tmp/mmap.bin", "w+")
# We'll write the dimensions of the array as the first two Ints in the file
write(s, size(A,1))
write(s, size(A,2))
# Now write the data
write(s, A)
close(s)

# Test by reading it back in
s = open("/tmp/mmap.bin")   # default is read-only
m = read(s, Int)
n = read(s, Int)
A2 = Mmap.mmap(s, Matrix{Int}, (m,n))
```

creates a `m`-by-`n` `Matrix{Int}`, linked to the file associated with stream `s`.

A more portable file would need to encode the word size -- 32 bit or 64 bit -- and endianness
information in the header. In practice, consider encoding binary data using standard formats
like HDF5 (which can be used with memory-mapping).
"""
Mmap.mmap(io, ::Type, dims, offset)

"""
    Mmap.mmap(io, BitArray, [dims, offset])

Create a `BitArray` whose values are linked to a file, using memory-mapping; it has the same
purpose, works in the same way, and has the same arguments, as [`mmap`](:func:`mmap`), but
the byte representation is different.

**Example**: `B = Mmap.mmap(s, BitArray, (25,30000))`

This would create a 25-by-30000 `BitArray`, linked to the file associated with stream `s`.
"""
Mmap.mmap(io, ::BitArray, dims = ?, offset = ?)

"""
    bessely0(x)

Bessel function of the second kind of order 0, ``Y_0(x)``.
"""
bessely0

"""
    any!(r, A)

Test whether any values in `A` along the singleton dimensions of `r` are `true`, and write
results to `r`.
"""
any!

"""
    filter!(function, collection)

Update `collection`, removing elements for which `function` is `false`. For associative
collections, the function is passed two arguments (key and value).

```jldoctest
julia> filter!(isodd, collect(1:10))
5-element Array{Int64,1}:
 1
 3
 5
 7
 9
```
"""
filter!

"""
    base64decode(string)

Decodes the base64-encoded `string` and returns a `Vector{UInt8}` of the decoded bytes.
"""
base64decode

"""
    oct(n, [pad])

Convert an integer to an octal string, optionally specifying a number of digits to pad to.
"""
oct

"""
    sizeof(T)

Size, in bytes, of the canonical binary representation of the given DataType `T`, if any.
"""
sizeof(::Type)

"""
    sizeof(s::AbstractString)

The number of bytes in string `s`.
"""
sizeof(::AbstractString)

"""
    ===(x, y)
    ≡(x,y)

See the [`is`](:func:`is`) operator.
"""
Base.:(===)

"""
    ReadOnlyMemoryError()

An operation tried to write to memory that is read-only.
"""
ReadOnlyMemoryError

"""
    startswith(string, prefix)

Returns `true` if `string` starts with `prefix`. If `prefix` is a vector or set
of characters, tests whether the first character of `string` belongs to that set.
"""
startswith

"""
    last(coll)

Get the last element of an ordered collection, if it can be computed in O(1) time. This is
accomplished by calling [`endof`](:func:`endof`) to get the last index. Returns the end
point of a [`Range`](:obj:`Range`) even if it is empty.
"""
last

"""
    islink(path) -> Bool

Returns `true` if `path` is a symbolic link, `false` otherwise.
"""
islink

"""
    bin(n, [pad])

Convert an integer to a binary string, optionally specifying a number of digits to pad to.
"""
bin

"""
    sinh(x)

Compute hyperbolic sine of `x`.
"""
sinh

"""
    ceil([T,] x, [digits, [base]])

`ceil(x)` returns the nearest integral value of the same type as `x` that is greater than or
equal to `x`.

`ceil(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is not
representable.

`digits` and `base` work as for [`round`](:func:`round`).
"""
ceil

"""
    issocket(path) -> Bool

Returns `true` if `path` is a socket, `false` otherwise.
"""
issocket

"""
    oftype(x, y)

Convert `y` to the type of `x` (`convert(typeof(x), y)`).
"""
oftype

"""
    maxabs!(r, A)

Compute the maximum absolute values over the singleton dimensions of `r`, and write values to `r`.
"""
maxabs!

"""
    isfinite(f) -> Bool

Test whether a number is finite
"""
isfinite

"""
    push!(collection, items...) -> collection

Insert one or more `items` at the end of `collection`.

```jldoctest
julia> push!([1, 2, 3], 4, 5, 6)
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6
```

Use [`append!`](:func:`append!`) to add all the elements of another collection to
`collection`. The result of the preceding example is equivalent to `append!([1, 2, 3], [4,
5, 6])`.
"""
push!

"""
    prevpow(a, x)

The largest `a^n` not greater than `x`, where `n` is a non-negative integer. `a` must be
greater than 1, and `x` must not be less than 1.
"""
prevpow

"""
    promote(xs...)

Convert all arguments to their common promotion type (if any), and return them all (as a tuple).
"""
promote

"""
    tan(x)

Compute tangent of `x`, where `x` is in radians.
"""
tan

"""
    sprint(f::Function, args...)

Call the given function with an I/O stream and the supplied extra arguments. Everything
written to this I/O stream is returned as a string.
"""
sprint

"""
    fd(stream)

Returns the file descriptor backing the stream or file. Note that this function only applies
to synchronous `File`'s and `IOStream`'s not to any of the asynchronous streams.
"""
fd

"""
    require(module::Symbol)

This function is part of the implementation of `using` / `import`, if a module is not
already defined in `Main`. It can also be called directly to force reloading a module,
regardless of whether it has been loaded before (for example, when interactively developing
libraries).

Loads a source files, in the context of the `Main` module, on every active node, searching
standard locations for files. `require` is considered a top-level operation, so it sets the
current `include` path but does not use it to search for files (see help for `include`).
This function is typically used to load library code, and is implicitly called by `using` to
load packages.

When searching for files, `require` first looks for package code under `Pkg.dir()`,
then tries paths in the global array `LOAD_PATH`. `require` is case-sensitive on
all platforms, including those with case-insensitive filesystems like macOS and
Windows.
"""
require

"""
    ones(type, dims)

Create an array of all ones of specified type. The type defaults to `Float64` if not specified.
"""
ones(t,dims)

"""
    ones(A)

Create an array of all ones with the same element type and shape as `A`.
"""
ones(A)

"""
    ind2chr(string, i)

Convert a byte index to a character index.
"""
ind2chr

"""
    reshape(A, dims)

Create an array with the same data as the given array, but with different dimensions.
"""
reshape

"""
    randsubseq!(S, A, p)

Like `randsubseq`, but the results are stored in `S` (which is resized as needed).
"""
randsubseq!

"""
    maximum(A, dims)

Compute the maximum value of an array over the given dimensions.
"""
maximum(A,dims)

"""
    redisplay(x)
    redisplay(d::Display, x)
    redisplay(mime, x)
    redisplay(d::Display, mime, x)

By default, the `redisplay` functions simply call `display`. However, some display backends
may override `redisplay` to modify an existing display of `x` (if any). Using `redisplay` is
also a hint to the backend that `x` may be redisplayed several times, and the backend may
choose to defer the display until (for example) the next interactive prompt.
"""
redisplay

"""
    searchsorted(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

Returns the range of indices of `a` which compare as equal to `x` according to the order
specified by the `by`, `lt` and `rev` keywords, assuming that `a` is already sorted in that
order. Returns an empty range located at the insertion point if `a` does not contain values
equal to `x`.
"""
searchsorted

"""
    /(x, y)

Right division operator: multiplication of `x` by the inverse of `y` on the right. Gives
floating-point results for integer arguments.
"""
Base.:(/)

"""
    connect([host],port) -> TCPSocket

Connect to the host `host` on port `port`.
"""
connect(host=?, port)

"""
    connect(path) -> PipeEndpoint

Connect to the named pipe / UNIX domain socket at `path`.
"""
connect(path)

"""
    split(string, [chars]; limit=0, keep=true)

Return an array of substrings by splitting the given string on occurrences of the given
character delimiters, which may be specified in any of the formats allowed by `search`'s
second argument (i.e. a single character, collection of characters, string, or regular
expression). If `chars` is omitted, it defaults to the set of all space characters, and
`keep` is taken to be `false`. The two keyword arguments are optional: they are a
maximum size for the result and a flag determining whether empty fields should be kept in
the result.
"""
split

"""
    dump(x)

Show every part of the representation of a value.
"""
dump

"""
    sumabs(itr)

Sum absolute values of all elements in a collection. This is equivalent to `sum(abs(itr))` but faster.
"""
sumabs(itr)

"""
    sumabs(A, dims)

Sum absolute values of elements of an array over the given dimensions.
"""
sumabs(A, dims)

"""
    consume(task, values...)

Receive the next value passed to `produce` by the specified task. Additional arguments may
be passed, to be returned from the last `produce` call in the producer.
"""
consume

"""
    cummax(A, [dim])

Cumulative maximum along a dimension. The dimension defaults to 1.
"""
cummax

"""
    watch_file(path, timeout_s::Real)

Watch file or directory `path` for changes until a change occurs or `timeout_s` seconds have
elapsed.

The returned value is an object with boolean fields `changed`, `renamed`, and `timedout`,
giving the result of watching the file.

This behavior of this function varies slightly across platforms. See
<https://nodejs.org/api/fs.html#fs_caveats> for more detailed information.
"""
watch_file

"""
    isinteractive() -> Bool

Determine whether Julia is running an interactive session.
"""
isinteractive

"""
    sum!(r, A)

Sum elements of `A` over the singleton dimensions of `r`, and write results to `r`.
"""
sum!

"""
    close(stream)

Close an I/O stream. Performs a `flush` first.
"""
close(stream::IO)

"""
    parentindexes(A)

From an array view `A`, returns the corresponding indexes in the parent.
"""
parentindexes

"""
    display(x)
    display(d::Display, x)
    display(mime, x)
    display(d::Display, mime, x)

Display `x` using the topmost applicable display in the display stack, typically using the
richest supported multimedia output for `x`, with plain-text `STDOUT` output as a fallback.
The `display(d, x)` variant attempts to display `x` on the given display `d` only, throwing
a `MethodError` if `d` cannot display objects of this type.

There are also two variants with a `mime` argument (a MIME type string, such as
`"image/png"`), which attempt to display `x` using the requested MIME type *only*, throwing
a `MethodError` if this type is not supported by either the display(s) or by `x`. With these
variants, one can also supply the "raw" data in the requested MIME type by passing
`x::AbstractString` (for MIME types with text-based storage, such as text/html or
application/postscript) or `x::Vector{UInt8}` (for binary MIME types).
"""
display

"""
    @spawnat

Accepts two arguments, `p` and an expression. A closure is created around the expression and
run asynchronously on process `p`. Returns a `Future` to the result.
"""
:@spawnat

"""
    print_shortest(io, x)

Print the shortest possible representation, with the minimum number of consecutive non-zero
digits, of number `x`, ensuring that it would parse to the exact same number.
"""
print_shortest

"""
    open(filename, [read, write, create, truncate, append]) -> IOStream

Open a file in a mode specified by five boolean arguments. The default is to open files for
reading only. Returns a stream for accessing the file.
"""
open(filename, ::Bool, ::Bool, ::Bool, ::Bool, ::Bool)

"""
    open(filename, [mode]) -> IOStream

Alternate syntax for open, where a string-based mode specifier is used instead of the five
booleans. The values of `mode` correspond to those from `fopen(3)` or Perl `open`, and are
equivalent to setting the following boolean groups:

| Mode | Description                   |
|:-----|:------------------------------|
| r    | read                          |
| r+   | read, write                   |
| w    | write, create, truncate       |
| w+   | read, write, create, truncate |
| a    | write, create, append         |
| a+   | read, write, create, append   |
"""
open(filename, mode="r")

"""
    open(f::Function, args...)

Apply the function `f` to the result of `open(args...)` and close the resulting file
descriptor upon completion.

**Example**: `open(readstring, "file.txt")`
"""
open(f::Function, args...)

"""
    kron(A, B)

Kronecker tensor product of two vectors or two matrices.
"""
kron

"""
    tuple(xs...)

Construct a tuple of the given objects.
"""
tuple

"""
    eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

Search for all matches of a the regular expression `r` in `s` and return a iterator over the
matches. If overlap is `true`, the matching sequences are allowed to overlap indices in the
original string, otherwise they must be from distinct character ranges.
"""
eachmatch

"""
    log10(x)

Compute the logarithm of `x` to base 10. Throws `DomainError` for negative `Real` arguments.
"""
log10

"""
    @profile

`@profile <expression>` runs your expression while taking periodic backtraces. These are
appended to an internal buffer of backtraces.
"""
:@profile

"""
    isdigit(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is a numeric digit (0-9), or whether this is true for all elements
of a string.
"""
isdigit

"""
    num2hex(f)

Get a hexadecimal string of the binary representation of a floating point number.
"""
num2hex

"""
    displayable(mime) -> Bool
    displayable(d::Display, mime) -> Bool

Returns a boolean value indicating whether the given `mime` type (string) is displayable by
any of the displays in the current display stack, or specifically by the display `d` in the
second variant.
"""
displayable

"""
    sdata(S::SharedArray)

Returns the actual `Array` object backing `S`.
"""
sdata

"""
    truncate(file,n)

Resize the file or buffer given by the first argument to exactly `n` bytes, filling
previously unallocated space with '\\0' if the file or buffer is grown.
"""
truncate

"""
    stat(file)

Returns a structure whose fields contain information about the file. The fields of the
structure are:

| Name    | Description                                                        |
|:--------|:-------------------------------------------------------------------|
| size    | The size (in bytes) of the file                                    |
| device  | ID of the device that contains the file                            |
| inode   | The inode number of the file                                       |
| mode    | The protection mode of the file                                    |
| nlink   | The number of hard links to the file                               |
| uid     | The user id of the owner of the file                               |
| gid     | The group id of the file owner                                     |
| rdev    | If this file refers to a device, the ID of the device it refers to |
| blksize | The file-system preferred block size for the file                  |
| blocks  | The number of such blocks allocated                                |
| mtime   | Unix timestamp of when the file was last modified                  |
| ctime   | Unix timestamp of when the file was created                        |

"""
stat

"""
    exp10(x)

Compute ``10^x``.
"""
exp10

"""
    &(x, y)

Bitwise and.
"""
&

"""
    PipeBuffer()

An IOBuffer that allows reading and performs writes by appending. Seeking and truncating are
not supported. See IOBuffer for the available constructors.
"""
PipeBuffer()

"""
    PipeBuffer(data::Vector{UInt8},[maxsize])

Create a PipeBuffer to operate on a data vector, optionally specifying a size beyond which
the underlying Array may not be grown.
"""
PipeBuffer(data)

"""
    cumsum!(B, A, [dim])

Cumulative sum of `A` along a dimension, storing the result in `B`. The dimension defaults
to 1.
"""
cumsum!

"""
    select(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

Variant of `select!` which copies `v` before partially sorting it, thereby returning the
same thing as `select!` but leaving `v` unmodified.
"""
select

"""
    lpad(string, n, p)

Make a string at least `n` columns wide when printed, by padding on the left with copies of `p`.
"""
lpad

"""
    accept(server[,client])

Accepts a connection on the given server and returns a connection to the client. An
uninitialized client stream may be provided, in which case it will be used instead of
creating a new stream.
"""
accept

"""
    readstring(stream::IO)
    readstring(filename::AbstractString)

Read the entire contents of an I/O stream or a file as a string.
The text is assumed to be encoded in UTF-8.
"""
readstring

"""
    poll_file(path, interval_s::Real, timeout_s::Real) -> (previous::StatStruct, current::StatStruct)

Monitor a file for changes by polling every `interval_s` seconds until a change occurs or
`timeout_s` seconds have elapsed. The `interval_s` should be a long period; the default is
5.007 seconds.

Returns a pair of `StatStruct` objects `(previous, current)` when a change is detected.

To determine when a file was modified, compare `mtime(prev) != mtime(current)` to detect
notification of changes. However, using `watch_file` for this operation is preferred, since
it is more reliable and efficient, although in some situations it may not be available.
"""
poll_file

"""
    eachline(stream::IO)
    eachline(filename::AbstractString)

Create an iterable object that will yield each line from an I/O stream or a file.
The text is assumed to be encoded in UTF-8.
"""
eachline

"""
    complex(r, [i])

Convert real numbers or arrays to complex. `i` defaults to zero.
"""
complex

"""
    setopt(sock::UDPSocket; multicast_loop = nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)

Set UDP socket options. `multicast_loop`: loopback for multicast packets (default: `true`).
`multicast_ttl`: TTL for multicast packets. `enable_broadcast`: flag must be set to `true`
if socket will be used for broadcast messages, or else the UDP system will return an access
error (default: `false`). `ttl`: Time-to-live of packets sent on the socket.
"""
setopt

"""
    Mmap.Anonymous(name, readonly, create)

Create an `IO`-like object for creating zeroed-out mmapped-memory that is not tied to a file
for use in `Mmap.mmap`. Used by `SharedArray` for creating shared memory arrays.
"""
Mmap.Anonymous

"""
    strwidth(s)

Gives the number of columns needed to print a string.
"""
strwidth

"""
    hex(n, [pad])

Convert an integer to a hexadecimal string, optionally specifying a number of digits to pad to.
"""
hex

"""
    workspace()

Replace the top-level module (`Main`) with a new one, providing a clean workspace. The
previous `Main` module is made available as `LastMain`. A previously-loaded package can be
accessed using a statement such as `using LastMain.Package`.

This function should only be used interactively.
"""
workspace

"""
    tempdir()

Obtain the path of a temporary directory (possibly shared with other processes).
"""
tempdir

"""
    mv(src::AbstractString,dst::AbstractString; remove_destination::Bool=false)

Move the file, link, or directory from `src` to `dst`. `remove_destination=true` will first
remove an existing `dst`.
"""
mv

"""
    erfi(x)

Compute the imaginary error function of `x`, defined by ``-i \\operatorname{erf}(ix)``.
"""
erfi

"""
    floor([T,] x, [digits, [base]])

`floor(x)` returns the nearest integral value of the same type as `x` that is less than or
equal to `x`.

`floor(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is
not representable.

`digits` and `base` work as for [`round`](:func:`round`).
"""
floor

"""
    ErrorException(msg)

Generic error type. The error message, in the `.msg` field, may provide more specific details.
"""
ErrorException

"""
    reverse(v [, start=1 [, stop=length(v) ]] )

Return a copy of `v` reversed from start to stop.
"""
reverse

"""
    reverse(s::AbstractString) -> AbstractString

Reverses a string.
"""
reverse(s::AbstractString)

"""
    reverse!(v [, start=1 [, stop=length(v) ]]) -> v

In-place version of [`reverse`](:func:`reverse`).
"""
reverse!

"""
    num(x)

Numerator of the rational representation of `x`.
"""
num

"""
    .<(x, y)

Element-wise less-than comparison operator.
"""
Base.:(.<)

"""
    UndefRefError()

The item or field is not defined for the given object.
"""
UndefRefError

"""
    bessely1(x)

Bessel function of the second kind of order 1, ``Y_1(x)``.
"""
bessely1

"""
    print(x)

Write (to the default output stream) a canonical (un-decorated) text representation of a
value if there is one, otherwise call `show`. The representation used by `print` includes
minimal formatting and tries to avoid Julia-specific details.
"""
print

"""
    filt(b, a, x, [si])

Apply filter described by vectors `a` and `b` to vector `x`, with an optional initial filter
state vector `si` (defaults to zeros).
"""
filt

"""
    indexpids(S::SharedArray)

Returns the index of the current worker into the `pids` vector, i.e., the list of workers
mapping the SharedArray
"""
indexpids

"""
    append!(collection, collection2) -> collection.

Add the elements of `collection2` to the end of `collection`.

```jldoctest
julia> append!([1],[2,3])
3-element Array{Int64,1}:
 1
 2
 3
```

```jldoctest
julia> append!([1, 2, 3], [4, 5, 6])
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6
```

Use [`push!`](:func:`push!`) to add individual items to `collection` which are not already
themselves in another collection. The result is of the preceding example is equivalent to
`push!([1, 2, 3], 4, 5, 6)`.
"""
append!

"""
    skip(s, offset)

Seek a stream relative to the current position.
"""
skip

"""
    lu(A) -> L, U, p

Compute the LU factorization of `A`, such that `A[p,:] = L*U`.
"""
lu

"""
    setdiff!(s, iterable)

Remove each element of `iterable` from set `s` in-place.
"""
setdiff!

"""
    isascii(c::Union{Char,AbstractString}) -> Bool

Tests whether a character belongs to the ASCII character set, or whether this is true for
all elements of a string.
"""
isascii

"""
    ucfirst(string)

Returns `string` with the first character converted to uppercase.
"""
ucfirst

"""
    copysign(x, y)

Return `x` such that it has the same sign as `y`
"""
copysign

"""
    getaddrinfo(host)

Gets the IP address of the `host` (may have to do a DNS lookup)
"""
getaddrinfo

"""
    @show

Show an expression and result, returning the result.
"""
:@show

"""
    showcompact(x)

Show a compact representation of a value.

This is used for printing array elements without repeating type information (which would
be redundant with that printed once for the whole array), and without line breaks inside
the representation of an element.

To offer a compact representation different from its standard one, a custom type should
test `get(io, :compact, false)` in its normal `show` method.
"""
showcompact

"""
    string(xs...)

Create a string from any values using the `print` function.
"""
string

"""
    erfc(x)

Compute the complementary error function of `x`, defined by ``1 - \\operatorname{erf}(x)``.
"""
erfc

"""
    getfield(value, name::Symbol)

Extract a named field from a `value` of composite type. The syntax `a.b` calls
`getfield(a, :b)`.
"""
getfield

"""
    besselj1(x)

Bessel function of the first kind of order 1, ``J_1(x)``.
"""
besselj1

"""
    select!(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

Partially sort the vector `v` in place, according to the order specified by `by`, `lt` and
`rev` so that the value at index `k` (or range of adjacent values if `k` is a range) occurs
at the position where it would appear if the array were fully sorted via a non-stable
algorithm. If `k` is a single index, that value is returned; if `k` is a range, an array of
values at those indices is returned. Note that `select!` does not fully sort the input
array.
"""
select!

"""
    maximum!(r, A)

Compute the maximum value of `A` over the singleton dimensions of `r`, and write results to `r`.
"""
maximum!

"""
    prod(itr)

Returns the product of all elements of a collection.
"""
prod(itr)

"""
    prod(A, dims)

Multiply elements of an array over the given dimensions.
"""
prod(A, dims)

"""
    log1p(x)

Accurate natural logarithm of `1+x`. Throws `DomainError` for `Real` arguments less than -1.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically
faster and more accurate.
"""
log1p

"""
    flipsign(x, y)

Return `x` with its sign flipped if `y` is negative. For example `abs(x) = flipsign(x,x)`.
"""
flipsign

"""
    lbeta(x, y)

Natural logarithm of the absolute value of the beta function ``\\log(|\\operatorname{B}(x,y)|)``.
"""
lbeta

"""
    randstring([rng,] len=8)

Create a random ASCII string of length `len`, consisting of upper- and
lower-case letters and the digits 0-9. The optional `rng` argument
specifies a random number generator, see [Random Numbers](:ref:`Random Numbers <random-numbers>`).
"""
randstring

"""
    Float64(x [, mode::RoundingMode])

Create a Float64 from `x`. If `x` is not exactly representable then `mode` determines how
`x` is rounded.

```jldoctest
julia> Float64(pi, RoundDown)
3.141592653589793

julia> Float64(pi, RoundUp)
3.1415926535897936
```

See [`RoundingMode`](:obj:`RoundingMode`) for available rounding modes.
"""
Float64

"""
    mkpath(path, [mode])

Create all directories in the given `path`, with permissions `mode`. `mode` defaults to
`0o777`, modified by the current file creation mask.
"""
mkpath

"""
    union(s1,s2...)
    ∪(s1,s2...)

Construct the union of two or more sets. Maintains order with arrays.
"""
union

"""
    lstat(file)

Like stat, but for symbolic links gets the info for the link itself rather than the file it
refers to. This function must be called on a file path rather than a file object or a file
descriptor.
"""
lstat

"""
    realmax(T)

The highest finite value representable by the given floating-point DataType `T`.
"""
realmax

"""
    takebuf_string(b::IOBuffer)

Obtain the contents of an `IOBuffer` as a string, without copying. Afterwards, the IOBuffer
is reset to its initial state.
"""
takebuf_string

"""
    serialize(stream, value)

Write an arbitrary value to a stream in an opaque format, such that it can be read back by
`deserialize`. The read-back value will be as identical as possible to the original. In
general, this process will not work if the reading and writing are done by different
versions of Julia, or an instance of Julia with a different system image. `Ptr` values are
serialized as all-zero bit patterns (`NULL`).
"""
serialize

"""
    sum(itr)

Returns the sum of all elements in a collection.
"""
sum(itr)

"""
    sum(A, dims)

Sum elements of an array over the given dimensions.
"""
sum(A, dims)

"""
    sum(f, itr)

Sum the results of calling function `f` on each element of `itr`.
"""
sum(f::Function, itr)

"""
    typemin(T)

The lowest value representable by the given (real) numeric DataType `T`.
"""
typemin

"""
    eof(stream) -> Bool

Tests whether an I/O stream is at end-of-file. If the stream is not yet exhausted, this
function will block to wait for more data if necessary, and then return `false`. Therefore
it is always safe to read one byte after seeing `eof` return `false`. `eof` will return
`false` as long as buffered data is still available, even if the remote end of a connection
is closed.
"""
eof

"""
    mktempdir([parent=tempdir()])

Create a temporary directory in the `parent` directory and return its path.
"""
mktempdir()

"""
    mktempdir(f::Function, [parent=tempdir()])

Apply the function `f` to the result of `mktempdir(parent)` and remove the temporary
directory upon completion.
"""
mktempdir(f::Function)

"""
    subtypes(T::DataType)

Return a list of immediate subtypes of DataType `T`. Note that all currently loaded subtypes
are included, including those not visible in the current module.
"""
subtypes

"""
    digits([T], n, [base], [pad])

Returns an array with element type `T` (default `Int`) of the digits of `n` in the given
base, optionally padded with zeros to a specified size. More significant digits are at
higher indexes, such that `n == sum([digits[k]*base^(k-1) for k=1:length(digits)])`.
"""
digits

"""
    bytes2hex(bin_arr::Array{UInt8, 1})

Convert an array of bytes to its hexadecimal representation. All characters are in
lower-case. Returns a `String`.
"""
bytes2hex

"""
    xcorr(u,v)

Compute the cross-correlation of two vectors.
"""
xcorr

"""
    typeof(x)

Get the concrete type of `x`.
"""
typeof

"""
    log(x)

Compute the natural logarithm of `x`. Throws `DomainError` for negative `Real` arguments.
Use complex negative arguments to obtain complex results.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically
faster and more accurate.
"""
log(x)

"""
    trunc([T,] x, [digits, [base]])

`trunc(x)` returns the nearest integral value of the same type as `x` whose absolute value
is less than or equal to `x`.

`trunc(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is
not representable.

`digits` and `base` work as for [`round`](:func:`round`).
"""
trunc

"""
    unsafe_convert(T,x)

Convert `x` to a value of type `T`

In cases where `convert` would need to take a Julia object and turn it into a `Ptr`, this
function should be used to define and perform that conversion.

Be careful to ensure that a Julia reference to `x` exists as long as the result of this
function will be used. Accordingly, the argument `x` to this function should never be an
expression, only a variable name or field reference. For example, `x=a.b.c` is acceptable,
but `x=[a,b,c]` is not.

The `unsafe` prefix on this function indicates that using the result of this function after
the `x` argument to this function is no longer accessible to the program may cause undefined
behavior, including program corruption or segfaults, at any later time.
"""
unsafe_convert

"""
    warn(msg)

Display a warning. Argument `msg` is a string describing the warning to be displayed.
"""
warn

"""
    erfinv(x)

Compute the inverse error function of a real `x`, defined by ``\\operatorname{erf}(\\operatorname{erfinv}(x)) = x``.
"""
erfinv

"""
    readdir([dir]) -> Vector{String}

Returns the files and directories in the directory `dir` (or the current working directory if not given).
"""
readdir

"""
    seek(s, pos)

Seek a stream to the given position.
"""
seek

"""
    instances(T::Type)

Return a collection of all instances of the given type, if applicable. Mostly used for
enumerated types (see `@enum`).
"""
instances

"""
    besselj0(x)

Bessel function of the first kind of order 0, ``J_0(x)``.
"""
besselj0

"""
    erfcinv(x)

Compute the inverse error complementary function of a real `x`, defined by
``\\operatorname{erfc}(\\operatorname{erfcinv}(x)) = x``.
"""
erfcinv

"""
    minabs(A, dims)

Compute the minimum absolute values over given dimensions.
"""
minabs(A, dims)

"""
    popdisplay()
    popdisplay(d::Display)

Pop the topmost backend off of the display-backend stack, or the topmost copy of `d` in the
second variant.
"""
popdisplay

"""
    filesize(path...)

Equivalent to `stat(file).size`.
"""
filesize

"""
    cglobal((symbol, library) [, type=Void])

Obtain a pointer to a global variable in a C-exported shared library, specified exactly as
in `ccall`. Returns a `Ptr{Type}`, defaulting to `Ptr{Void}` if no Type argument is
supplied. The values can be read or written by `unsafe_load` or `unsafe_store!`,
respectively.
"""
cglobal

"""
    one(x)

Get the multiplicative identity element for the type of `x` (`x` can also specify the type
itself). For matrices, returns an identity matrix of the appropriate size and type.
"""
one

"""
    endof(collection) -> Integer

Returns the last index of the collection.

```jldoctest
julia> endof([1,2,4])
3
```
"""
endof

"""
    isfifo(path) -> Bool

Returns `true` if `path` is a FIFO, `false` otherwise.
"""
isfifo

"""
    Channel{T}(sz::Int)

Constructs a `Channel` that can hold a maximum of `sz` objects of type `T`. `put!` calls on
a full channel block till an object is removed with `take!`.

Other constructors:

- `Channel()` - equivalent to `Channel{Any}(32)`
- `Channel(sz::Int)` equivalent to `Channel{Any}(sz)`
"""
Channel

"""
    next(iter, state) -> item, state

For a given iterable object and iteration state, return the current item and the next iteration state.
"""
next

"""
    log2(x)

Compute the logarithm of `x` to base 2. Throws `DomainError` for negative `Real` arguments.
"""
log2

"""
    Base64EncodePipe(ostream)

Returns a new write-only I/O stream, which converts any bytes written to it into
base64-encoded ASCII bytes written to `ostream`. Calling `close` on the `Base64EncodePipe` stream
is necessary to complete the encoding (but does not close `ostream`).
"""
Base64EncodePipe

"""
    issetgid(path) -> Bool

Returns `true` if `path` has the setgid flag set, `false` otherwise.
"""
issetgid

"""
    isnull(x)

Is the `Nullable` object `x` null, i.e. missing a value?
"""
isnull

"""
    abs2(x)

Squared absolute value of `x`.
"""
abs2

"""
    sizehint!(s, n)

Suggest that collection `s` reserve capacity for at least `n` elements. This can improve performance.
"""
sizehint!

"""
    isgraph(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is printable, and not a space, or whether this is true for all
elements of a string. Any character that would cause a printer to use ink should be
classified with `isgraph(c)==true`.
"""
isgraph

"""
    OutOfMemoryError()

An operation allocated too much memory for either the system or the garbage collector to
handle properly.
"""
OutOfMemoryError

"""
    versioninfo([verbose::Bool])

Print information about the version of Julia in use. If the `verbose` argument is `true`,
detailed system information is shown as well.
"""
versioninfo

"""
    gperm(file)

Like uperm but gets the permissions of the group owning the file.
"""
gperm

"""
    nb_available(stream)

Returns the number of bytes available for reading before a read from this stream or buffer will block.
"""
nb_available

"""
    finalize(x)

Immediately run finalizers registered for object `x`.
"""
finalize

"""
    base(base, n, [pad])

Convert an integer to a string in the given base, optionally specifying a number of digits to pad to.
"""
base

"""
    BoundsError([a],[i])

An indexing operation into an array, `a`, tried to access an out-of-bounds element, `i`.
"""
BoundsError

"""
    conv2(u,v,A)

2-D convolution of the matrix `A` with the 2-D separable kernel generated by the vectors `u`
and `v`. Uses 2-D FFT algorithm.
"""
conv2(u, v, A)

"""
    conv2(B,A)

2-D convolution of the matrix `B` with the matrix `A`. Uses 2-D FFT algorithm.
"""
conv2(B, A)

"""
    invoke(f, (types...), args...)

Invoke a method for the given generic function matching the specified types (as a tuple), on
the specified arguments. The arguments must be compatible with the specified types. This
allows invoking a method other than the most specific matching method, which is useful when
the behavior of a more general definition is explicitly needed (often as part of the
implementation of a more specific method of the same function).
"""
invoke

"""
    parse(str, start; greedy=true, raise=true)

Parse the expression string and return an expression (which could later be passed to eval
for execution). `start` is the index of the first character to start parsing. If `greedy` is
`true` (default), `parse` will try to consume as much input as it can; otherwise, it will
stop as soon as it has parsed a valid expression. Incomplete but otherwise syntactically
valid expressions will return `Expr(:incomplete, "(error message)")`. If `raise` is `true`
(default), syntax errors other than incomplete expressions will raise an error. If `raise`
is `false`, `parse` will return an expression that will raise an error upon evaluation.
"""
parse(str, start)

"""
    parse(str; raise=true)

Parse the expression string greedily, returning a single expression. An error is thrown if
there are additional characters after the first expression. If `raise` is `true` (default),
syntax errors will raise an error; otherwise, `parse` will return an expression that will
raise an error upon evaluation.
"""
parse(str)

"""
    parse(type, str, [base])

Parse a string as a number. If the type is an integer type, then a base can be specified
(the default is 10). If the type is a floating point type, the string is parsed as a decimal
floating point number. If the string does not contain a valid number, an error is raised.
"""
parse(T::Type, str, base=Int)

"""
    touch(path::AbstractString)

Update the last-modified timestamp on a file to the current time.
"""
touch

"""
    bkfact!(A) -> BunchKaufman

`bkfact!` is the same as [`bkfact`](:func:`bkfact`), but saves space by overwriting the
input `A`, instead of creating a copy.
"""
bkfact!

"""
    ^(x, y)

Exponentiation operator.
"""
Base.:(^)(x, y)

"""
    ^(s, n)

Repeat `n` times the string `s`. The `repeat` function is an alias to this operator.

```jldoctest
julia> "Test "^3
"Test Test Test "
```
"""
Base.:(^)(s::AbstractString, n::Int)

"""
    position(s)

Get the current position of a stream.
"""
position

"""
    selectperm(v, k, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

Return a partial permutation of the vector `v`, according to the order specified by
`by`, `lt` and `rev`, so that `v[output]` returns the first `k` (or range of adjacent values
if `k` is a range) values of a fully sorted version of `v`. If `k` is a single index
(Integer), an array of the first `k` indices is returned; if `k` is a range, an array of
those indices is returned. Note that the handling of integer values for `k` is different
from `select` in that it returns a vector of `k` elements instead of just the `k` th
element. Also note that this is equivalent to, but more efficient than, calling
`sortperm(...)[k]`
"""
selectperm

"""
    isabspath(path::AbstractString) -> Bool

Determines whether a path is absolute (begins at the root directory).
"""
isabspath

"""
    hex2bytes(s::AbstractString)

Convert an arbitrarily long hexadecimal string to its binary representation. Returns an
`Array{UInt8,1}`, i.e. an array of bytes.
"""
hex2bytes

"""
    isdir(path) -> Bool

Returns `true` if `path` is a directory, `false` otherwise.
"""
isdir

"""
    reinterpret(type, A)

Change the type-interpretation of a block of memory. For example,
`reinterpret(Float32, UInt32(7))` interprets the 4 bytes corresponding to `UInt32(7)` as a
`Float32`. For arrays, this constructs an array with the same binary data as the given
array, but with the specified element type.
"""
reinterpret

"""
    ~(x)

Bitwise not.
"""
~

"""
    info(msg)

Display an informational message. Argument `msg` is a string describing the information to be displayed.
"""
info

"""
    ltoh(x)

Converts the endianness of a value from Little-endian to that used by the Host.
"""
ltoh

"""
    evalfile(path::AbstractString)

Load the file using `include`, evaluate all expressions, and return the value of the last one.
"""
evalfile

"""
    normalize_string(s, normalform::Symbol)

Normalize the string `s` according to one of the four "normal forms" of the Unicode
standard: `normalform` can be `:NFC`, `:NFD`, `:NFKC`, or `:NFKD`.  Normal forms C
(canonical composition) and D (canonical decomposition) convert different visually identical
representations of the same abstract string into a single canonical form, with form C being
more compact.  Normal forms KC and KD additionally canonicalize "compatibility equivalents":
they convert characters that are abstractly similar but visually distinct into a single
canonical choice (e.g. they expand ligatures into the individual characters), with form KC
being more compact.

Alternatively, finer control and additional transformations may be be obtained by calling
`normalize_string(s; keywords...)`, where any number of the following boolean keywords
options (which all default to `false` except for `compose`) are specified:

* `compose=false`: do not perform canonical composition
* `decompose=true`: do canonical decomposition instead of canonical composition (`compose=true`
  is ignored if present)
* `compat=true`: compatibility equivalents are canonicalized
* `casefold=true`: perform Unicode case folding, e.g. for case-insensitive string comparison
* `newline2lf=true`, `newline2ls=true`, or `newline2ps=true`: convert various newline sequences
  (LF, CRLF, CR, NEL) into a linefeed (LF), line-separation (LS), or paragraph-separation (PS)
  character, respectively
* `stripmark=true`: strip diacritical marks (e.g. accents)
* `stripignore=true`: strip Unicode's "default ignorable" characters (e.g. the soft hyphen
  or the left-to-right marker)
* `stripcc=true`: strip control characters; horizontal tabs and form feeds are converted to
  spaces; newlines are also converted to spaces unless a newline-conversion flag was specified
* `rejectna=true`: throw an error if unassigned code points are found
* `stable=true`: enforce Unicode Versioning Stability

For example, NFKC corresponds to the options `compose=true, compat=true, stable=true`.
"""
normalize_string

"""
    cd([dir::AbstractString=homedir()])

Set the current working directory.
"""
cd(dir::AbstractString)

"""
    cd(f, [dir=homedir()])

Temporarily changes the current working directory and applies function `f` before returning.
"""
cd(f, dir=?)

"""
    hton(x)

Converts the endianness of a value from that used by the Host to Network byte order (big-endian).
"""
hton

"""
    mark(s)

Add a mark at the current position of stream `s`. Returns the marked position.

See also [`unmark`](:func:`unmark`), [`reset`](:func:`reset`), [`ismarked`](:func:`ismarked`).
"""
mark

"""
    cp(src::AbstractString, dst::AbstractString; remove_destination::Bool=false, follow_symlinks::Bool=false)

Copy the file, link, or directory from *src* to *dest*. `remove_destination=true` will first
remove an existing `dst`.

If `follow_symlinks=false`, and `src` is a symbolic link, `dst` will be created as a
symbolic link. If `follow_symlinks=true` and `src` is a symbolic link, `dst` will be a copy
of the file or directory `src` refers to.
"""
cp

"""
    bswap(n)

Byte-swap an integer.
"""
bswap

"""
    sumabs2!(r, A)

Sum squared absolute values of elements of `A` over the singleton dimensions of `r`, and
write results to `r`.
"""
sumabs2!

"""
    IPv4(host::Integer) -> IPv4

Returns IPv4 object from ip address formatted as Integer.
"""
IPv4

"""
    isalnum(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is alphanumeric, or whether this is true for all elements of a
string. A character is classified as alphabetic if it belongs to the Unicode general
category Letter or Number, i.e. a character whose category code begins with 'L' or 'N'.
"""
isalnum

"""
    @sprintf("%Fmt", args...)

Return `@printf` formatted output as string.

    julia> s = @sprintf "this is a %s %15.1f" "test" 34.567;

    julia> println(s)
    this is a test            34.6
"""
:@sprintf

"""
    tanh(x)

Compute hyperbolic tangent of `x`.
"""
tanh

"""
    repr(x)

Create a string from any value using the `showall` function.
"""
repr

"""
    maxintfloat(T)

The largest integer losslessly representable by the given floating-point DataType `T`.
"""
maxintfloat

"""
    delete!(collection, key)

Delete the mapping for the given key in a collection, and return the collection.
"""
delete!

"""
    chr2ind(string, i)

Convert a character index to a byte index.
"""
chr2ind

"""
    isreadable(io) -> Bool

Returns `true` if the specified IO object is readable (if that can be determined).
"""
isreadable

"""
    eps(T)

The distance between 1.0 and the next larger representable floating-point value of
`DataType` `T`. Only floating-point types are sensible arguments.
"""
eps(::Union{Type{BigFloat},Type{Float64},Type{Float32},Type{Float16}})

"""
    eps()

The distance between 1.0 and the next larger representable floating-point value of `Float64`.
"""
eps()

"""
    eps(x)

The distance between `x` and the next larger representable floating-point value of the same
`DataType` as `x`.
"""
eps(::AbstractFloat)

"""
    isalpha(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is alphabetic, or whether this is true for all elements of a
string. A character is classified as alphabetic if it belongs to the Unicode general
category Letter, i.e. a character whose category code begins with 'L'.
"""
isalpha

"""
    searchsortedfirst(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

Returns the index of the first value in `a` greater than or equal to `x`, according to the
specified order. Returns `length(a)+1` if `x` is greater than all values in `a`.
"""
searchsortedfirst

"""
    big(x)

Convert a number to a maximum precision representation (typically `BigInt` or `BigFloat`).
See `BigFloat` for information about some pitfalls with floating-point numbers.
"""
big

"""
    quit()

Quit the program indicating that the processes completed successfully. This function calls
`exit(0)` (see [`exit`](:func:`exit`)).
"""
quit

"""
    escape_string(io, str::AbstractString, esc::AbstractString)

General escaping of traditional C and Unicode escape sequences, plus any characters in esc
are also escaped (with a backslash).
"""
escape_string(io, str, esc)

"""
    typejoin(T, S)

Compute a type that contains both `T` and `S`.
"""
typejoin

"""
    Base64DecodePipe(istream)

Returns a new read-only I/O stream, which decodes base64-encoded data read from `istream`.
"""
Base64DecodePipe

"""
    sum_kbn(A)

Returns the sum of all array elements, using the Kahan-Babuska-Neumaier compensated
summation algorithm for additional accuracy.
"""
sum_kbn

"""
    beta(x, y)

Euler integral of the first kind ``\\operatorname{B}(x,y) = \\Gamma(x)\\Gamma(y)/\\Gamma(x+y)``.
"""
beta

"""
    include_string(code::AbstractString, [filename])

Like `include`, except reads code from the given string rather than from a file. Since there
is no file path involved, no path processing or fetching from node 1 is done.
"""
include_string

"""
    chmod(path, mode; recursive=false)

Change the permissions mode of `path` to `mode`. Only integer `mode`s (e.g. `0o777`) are
currently supported. If `recursive=true` and the path is a directory all permissions in
that directory will be recursively changed.
"""
chmod

"""
    chown(path, owner, group=-1)

Change the owner and/or group of `path` to `owner` and/or `group`. If the value entered for `owner` or `group`
is `-1` the corresponding ID will not change. Only integer `owner`s and `group`s are currently supported.
"""
chown

"""
    sin(x)

Compute sine of `x`, where `x` is in radians.
"""
sin

"""
    Base.compilecache(module::String)

Creates a precompiled cache file for module (see help for `require`) and all of its
dependencies. This can be used to reduce package load times. Cache files are stored in
`LOAD_CACHE_PATH[1]`, which defaults to `~/.julia/lib/VERSION`. See
[Module initialization and precompilation](:ref:`Module initialization and precompilation <man-modules-initialization-precompilation>`)
for important notes.
"""
compilecache

"""
    clipboard() -> AbstractString

Return a string with the contents of the operating system clipboard ("paste").
"""
clipboard

"""
    clipboard(x)

Send a printed form of `x` to the operating system clipboard ("copy").
"""
clipboard(x)

"""
    A_mul_B!(Y, A, B) -> Y

Calculates the matrix-matrix or matrix-vector product ``A⋅B`` and stores the result in `Y`,
overwriting the existing value of `Y`. Note that `Y` must not be aliased with either `A` or
`B`.

```jldoctest
julia> A=[1.0 2.0; 3.0 4.0]; B=[1.0 1.0; 1.0 1.0]; Y = similar(B); A_mul_B!(Y, A, B);

julia> Y
2×2 Array{Float64,2}:
 3.0  3.0
 7.0  7.0
```
"""
A_mul_B!

"""
    ntuple(f::Function, n)

Create a tuple of length `n`, computing each element as `f(i)`, where `i` is the index of the element.
"""
ntuple

"""
    selectperm!(ix, v, k, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false,] [initialized=false])

Like `selectperm`, but accepts a preallocated index vector `ix`. If `initialized` is `false`
(the default), ix is initialized to contain the values `1:length(ix)`.
"""
selectperm!

"""
    search(string, chars, [start])

Search for the first occurrence of the given characters within the given string. The second
argument may be a single character, a vector or a set of characters, a string, or a regular
expression (though regular expressions are only allowed on contiguous strings, such as ASCII
or UTF-8 strings). The third argument optionally specifies a starting index. The return
value is a range of indexes where the matching sequence is found, such that `s[search(s,x)] == x`:

`search(string, "substring")` = `start:end` such that `string[start:end] == "substring"`, or
`0:-1` if unmatched.

`search(string, 'c')` = `index` such that `string[index] == 'c'`, or `0` if unmatched.
"""
search

"""
    contains(haystack, needle)

Determine whether the second argument is a substring of the first.
"""
contains

"""
    flush(stream)

Commit all currently buffered writes to the given stream.
"""
flush

"""
    precompile(f,args::Tuple{Vararg{Any}})

Compile the given function `f` for the argument tuple (of types) `args`, but do not execute it.
"""
precompile

"""
    asinh(x)

Compute the inverse hyperbolic sine of `x`.
"""
asinh

"""
    atreplinit(f)

Register a one-argument function to be called before the REPL interface is initialized in
interactive sessions; this is useful to customize the interface. The argument of `f` is the
REPL object. This function should be called from within the `.juliarc.jl` initialization
file.
"""
atreplinit

"""
    strip(string, [chars])

Return `string` with any leading and trailing whitespace removed. If `chars` (a character,
or vector or set of characters) is provided, instead remove characters contained in it.
"""
strip

"""
    minimum(A, dims)

Compute the minimum value of an array over the given dimensions.
"""
minimum(A,dims)

"""
    var(v[, region])

Compute the sample variance of a vector or array `v`, optionally along dimensions in
`region`. The algorithm will return an estimator of the generative distribution's variance
under the assumption that each entry of `v` is an IID drawn from that generative
distribution. This computation is equivalent to calculating `sumabs2(v - mean(v)) /
(length(v) - 1)`. Note: Julia does not ignore `NaN` values in the computation. For
applications requiring the handling of missing data, the `DataArray` package is recommended.
"""
var

"""
    lcfirst(string)

Returns `string` with the first character converted to lowercase.
"""
lcfirst

"""
    readlink(path) -> AbstractString

Returns the value of a symbolic link `path`.
"""
readlink

"""
    redirect_stdin([stream])

Like redirect_stdout, but for STDIN. Note that the order of the return tuple is still
(rd,wr), i.e. data to be read from STDIN, may be written to wr.
"""
redirect_stdin

"""
    mktemp([parent=tempdir()])

Returns `(path, io)`, where `path` is the path of a new temporary file in `parent` and `io`
is an open file object for this path.
"""
mktemp(?)

"""
    mktemp(f::Function, [parent=tempdir()])

Apply the function `f` to the result of `mktemp(parent)` and remove the temporary file upon completion.
"""
mktemp(::Function, ?)

"""
    isreadonly(stream) -> Bool

Determine whether a stream is read-only.
"""
isreadonly

"""
    view(A, inds...)

Like [`getindex`](:func:`getindex`), but returns a view into the parent array `A` with the
given indices instead of making a copy.  Calling [`getindex`](:func:`getindex`) or
[`setindex!`](:func:`setindex!`) on the returned [`SubArray`](:obj:`SubArray`) computes the
indices to the parent array on the fly without checking bounds.
"""
view

"""
    expanduser(path::AbstractString) -> AbstractString

On Unix systems, replace a tilde character at the start of a path with the current user's home directory.
"""
expanduser

"""
    cot(x)

Compute the cotangent of `x`, where `x` is in radians.
"""
cot

"""
    get(collection, key, default)

Return the value stored for the given key, or the given default value if no mapping for the
key is present.
"""
get(collection,key,default)

"""
    get(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, return
`f()`.  Use [`get!`](:func:`get!`) to also store the default value in the dictionary.

This is intended to be called using `do` block syntax

```julia
get(dict, key) do
    # default value calculated here
    time()
end
```
"""
get

"""
    lufact!(A) -> LU

`lufact!` is the same as [`lufact`](:func:`lufact`), but saves space by overwriting the
input `A`, instead of creating a copy. An `InexactError` exception is thrown if the
factorisation produces a number not representable by the element type of `A`, e.g. for
integer types.
"""
lufact!

"""
    IOBuffer() -> IOBuffer

Create an in-memory I/O stream.
"""
IOBuffer()

"""
    IOBuffer(size::Int)

Create a fixed size IOBuffer. The buffer will not grow dynamically.
"""
IOBuffer(size::Int)

"""
    IOBuffer(string)

Create a read-only IOBuffer on the data underlying the given string.
"""
IOBuffer(::AbstractString)

"""
    IOBuffer([data,],[readable,writable,[maxsize]])

Create an IOBuffer, which may optionally operate on a pre-existing array. If the
readable/writable arguments are given, they restrict whether or not the buffer may be read
from or written to respectively. By default the buffer is readable but not writable. The
last argument optionally specifies a size beyond which the buffer may not be grown.
"""
IOBuffer(data=?)

"""
    tempname()

Generate a unique temporary file path.
"""
tempname

"""
    poll_fd(fd, timeout_s::Real; readable=false, writable=false)

Monitor a file descriptor `fd` for changes in the read or write availability, and with a
timeout given by `timeout_s` seconds.

The keyword arguments determine which of read and/or write status should be monitored; at
least one of them must be set to `true`.

The returned value is an object with boolean fields `readable`, `writable`, and `timedout`,
giving the result of the polling.
"""
poll_fd

"""
    Mmap.sync!(array)

Forces synchronization between the in-memory version of a memory-mapped `Array` or
`BitArray` and the on-disk version.
"""
Mmap.sync!

"""
    csc(x)

Compute the cosecant of `x`, where `x` is in radians.
"""
csc

"""
    hash(x[, h::UInt])

Compute an integer hash code such that `isequal(x,y)` implies `hash(x)==hash(y)`. The
optional second argument `h` is a hash code to be mixed with the result.

New types should implement the 2-argument form, typically by calling the 2-argument `hash`
method recursively in order to mix hashes of the contents with each other (and with `h`).
Typically, any type that implements `hash` should also implement its own `==` (hence
`isequal`) to guarantee the property mentioned above.
"""
hash

"""
    send(socket::UDPSocket, host::IPv4, port::Integer, msg)

Send `msg` over `socket` to `host:port`.
"""
send

"""
    atanh(x)

Compute the inverse hyperbolic tangent of `x`.
"""
atanh

"""
    read(stream::IO, T)

Read a single value of type `T` from `stream`, in canonical binary representation.
"""
read(stream, t)

"""
    read(stream::IO, T, dims)

Read a series of values of type `T` from `stream`, in canonical binary representation.
`dims` is either a tuple or a series of integer arguments specifying the size of the `Array{T}`
to return.
"""
read(stream, t, dims)

"""
    read(filename::AbstractString, args...)

Open a file and read its contents. `args` is passed to `read`: this is equivalent to
`open(io->read(io, args...), filename)`.
"""
read(filename, args...)

"""
    isopen(object) -> Bool

Determine whether an object - such as a stream, timer, or mmap -- is not yet closed. Once an
object is closed, it will never produce a new event. However, a closed stream may still have
data to read in its buffer, use `eof` to check for the ability to read data. Use `poll_fd`
to be notified when a stream might be writable or readable.
"""
isopen

"""
    shift!(collection) -> item

Remove the first `item` from `collection`.

```jldoctest
julia> A = [1, 2, 3, 4, 5, 6]
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6

julia> shift!(A)
1

julia> A
5-element Array{Int64,1}:
 2
 3
 4
 5
 6
```
"""
shift!

"""
    spawn(command)

Run a command object asynchronously, returning the resulting `Process` object.
"""
spawn

"""
    isposdef(A) -> Bool

Test whether a matrix is positive definite.
"""
isposdef

"""
    nextind(str, i)

Get the next valid string index after `i`. Returns a value greater than `endof(str)` at or
after the end of the string.
"""
nextind

"""
    eta(x)

Dirichlet eta function ``\\eta(s) = \\sum^\\infty_{n=1}(-1)^{n-1}/n^{s}``.
"""
eta

"""
    isdefined([m::Module,] s::Symbol)
    isdefined(object, s::Symbol)
    isdefined(a::AbstractArray, index::Int)

Tests whether an assignable location is defined. The arguments can be a module and a symbol,
a composite object and field name (as a symbol), or an array and index. With a single
symbol argument, tests whether a global variable with that name is defined in
`current_module()`.
"""
isdefined

"""
    cotd(x)

Compute the cotangent of `x`, where `x` is in degrees.
"""
cotd

"""
    dec(n, [pad])

Convert an integer to a decimal string, optionally specifying a number of digits to pad to.
"""
dec

"""
    wait([x])

Block the current task until some event occurs, depending on the type of the argument:

* `RemoteChannel` : Wait for a value to become available on the specified remote channel.
* `Future` : Wait for a value to become available for the specified future.
* `Channel`: Wait for a value to be appended to the channel.
* `Condition`: Wait for `notify` on a condition.
* `Process`: Wait for a process or process chain to exit. The `exitcode` field of a process
  can be used to determine success or failure.
* `Task`: Wait for a `Task` to finish, returning its result value. If the task fails with an
  exception, the exception is propagated (re-thrown in the task that called `wait`).
* `RawFD`: Wait for changes on a file descriptor (see `poll_fd` for keyword arguments and return code)

If no argument is passed, the task blocks for an undefined period. A task can only be
restarted by an explicit call to `schedule` or `yieldto`.

Often `wait` is called within a `while` loop to ensure a waited-for condition is met before proceeding.
"""
wait

"""
    atexit(f)

Register a zero-argument function `f()` to be called at process exit. `atexit()` hooks are
called in last in first out (LIFO) order and run before object finalizers.
"""
atexit

"""
    readchomp(x)

Read the entirety of `x` as a string and remove a single trailing newline. Equivalent to `chomp(readstring(x))`.
"""
readchomp

"""
    readbytes!(stream::IO, b::AbstractVector{UInt8}, nb=length(b); all=true)

Read at most `nb` bytes from `stream` into `b`, returning the number of bytes read.
The size of `b` will be increased if needed (i.e. if `nb` is greater than `length(b)`
and enough bytes could be read), but it will never be decreased.

See `read` for a description of the `all` option.
"""
readbytes!

"""
    basename(path::AbstractString) -> AbstractString

Get the file name part of a path.
"""
basename

"""
    isnumber(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is numeric, or whether this is true for all elements of a string.
A character is classified as numeric if it belongs to the Unicode general category Number,
i.e. a character whose category code begins with 'N'.
"""
isnumber

"""
    copy(x)

Create a shallow copy of `x`: the outer structure is copied, but not all internal values.
For example, copying an array produces a new array with identically-same elements as the
original.
"""
copy

"""
    isempty(collection) -> Bool

Determine whether a collection is empty (has no elements).

```jldoctest
julia> isempty([])
true

julia> isempty([1 2 3])
false
```
"""
isempty

"""
    sumabs!(r, A)

Sum absolute values of elements of `A` over the singleton dimensions of `r`, and write
results to `r`.
"""
sumabs!

"""
    htol(x)

Converts the endianness of a value from that used by the Host to Little-endian.
"""
htol

"""
    ctime(file)

Equivalent to `stat(file).ctime`
"""
ctime

"""
    normpath(path::AbstractString) -> AbstractString

Normalize a path, removing "." and ".." entries.
"""
normpath

"""
    unmark(s)

Remove a mark from stream `s`. Returns `true` if the stream was marked, `false` otherwise.

See also [`mark`](:func:`mark`), [`reset`](:func:`reset`), [`ismarked`](:func:`ismarked`).
"""
unmark

"""
    reset(s)

Reset a stream `s` to a previously marked position, and remove the mark. Returns the
previously marked position. Throws an error if the stream is not marked.

See also [`mark`](:func:`mark`), [`unmark`](:func:`unmark`), [`ismarked`](:func:`ismarked`).
"""
reset

"""
    hex2num(str)

Convert a hexadecimal string to the floating point number it represents.
"""
hex2num

"""
    InexactError()

Type conversion cannot be done exactly.
"""
InexactError

"""
    typemax(T)

The highest value representable by the given (real) numeric `DataType`.
"""
typemax

"""
    all(A, dims)

Test whether all values along the given dimensions of an array are `true`.
"""
all(A::AbstractArray, dims)

"""
    bind(socket::Union{UDPSocket, TCPSocket}, host::IPAddr, port::Integer; ipv6only=false)

Bind `socket` to the given `host:port`. Note that `0.0.0.0` will listen on all devices.
`ipv6only` parameter disables dual stack mode. If it's `true`, only IPv6 stack is created.
"""
bind

"""
    cld(x, y)

Smallest integer larger than or equal to `x/y`.
"""
cld

"""
    issetuid(path) -> Bool

Returns `true` if `path` has the setuid flag set, `false` otherwise.
"""
issetuid

"""
    DomainError()

The arguments to a function or constructor are outside the valid domain.
"""
DomainError

"""
    acosh(x)

Compute the inverse hyperbolic cosine of `x`.
"""
acosh

"""
    IntSet([itr])

Construct a sorted set of positive `Int`s generated by the given iterable object, or an
empty set. Implemented as a bit string, and therefore designed for dense integer sets. Only
`Int`s greater than 0 can be stored. If the set will be sparse (for example holding a few
very large integers), use [`Set`](:obj:`Set`) instead.
"""
IntSet

"""
    Task(func)

Create a `Task` (i.e. coroutine) to execute the given function (which must be
callable with no arguments). The task exits when this function returns.
"""
Task

"""
    pushdisplay(d::Display)

Pushes a new display `d` on top of the global display-backend stack. Calling `display(x)` or
`display(mime, x)` will display `x` on the topmost compatible backend in the stack (i.e.,
the topmost backend that does not throw a `MethodError`).
"""
pushdisplay

"""
    prevind(str, i)

Get the previous valid string index before `i`. Returns a value less than `1` at the
beginning of the string.
"""
prevind

"""
    lowercase(string)

Returns `string` with all characters converted to lowercase.
"""
lowercase

"""
    produce(value)

Send the given value to the last `consume` call, switching to the consumer task. If the next
`consume` call passes any values, they are returned by `produce`.
"""
produce

"""
    StackOverflowError()

The function call grew beyond the size of the call stack. This usually happens when a call
recurses infinitely.
"""
StackOverflowError

"""
    BigInt(x)

Create an arbitrary precision integer. `x` may be an `Int` (or anything that can be
converted to an `Int`).  The usual mathematical operators are defined for this type, and
results are promoted to a `BigInt`.

Instances can be constructed from strings via [`parse`](:func:`parse`), or using the `big`
string literal.
"""
BigInt

"""
    rsearch(string, chars, [start])

Similar to `search`, but returning the last occurrence of the given characters within the
given string, searching in reverse from `start`.
"""
rsearch

"""
    isdirpath(path::AbstractString) -> Bool

Determines whether a path refers to a directory (for example, ends with a path separator).
"""
isdirpath

"""
    in(item, collection) -> Bool
    ∈(item,collection) -> Bool
    ∋(collection,item) -> Bool
    ∉(item,collection) -> Bool
    ∌(collection,item) -> Bool

Determine whether an item is in the given collection, in the sense that it is `==` to one of
the values generated by iterating over the collection. Some collections need a slightly
different definition; for example [`Set`](:obj:`Set`)s check whether the item
[`isequal`](:func:`isequal`) to one of the elements. [`Dict`](:obj:`Dict`)s look for
`(key,value)` pairs, and the key is compared using [`isequal`](:func:`isequal`). To test for
the presence of a key in a dictionary, use [`haskey`](:func:`haskey`) or `k in keys(dict)`.
"""
Base.in

"""
    isblockdev(path) -> Bool

Returns `true` if `path` is a block device, `false` otherwise.
"""
isblockdev

"""
    ==(x, y)

Generic equality operator, giving a single `Bool` result. Falls back to `===`. Should be
implemented for all types with a notion of equality, based on the abstract value that an
instance represents. For example, all numeric types are compared by numeric value, ignoring
type. Strings are compared as sequences of characters, ignoring encoding.

Follows IEEE semantics for floating-point numbers.

Collections should generally implement `==` by calling `==` recursively on all contents.

New numeric types should implement this function for two arguments of the new type, and
handle comparison to other types via promotion rules where possible.
"""
Base.:(==)

"""
    seekstart(s)

Seek a stream to its beginning.
"""
seekstart

"""
    nfields(x::DataType) -> Int

Get the number of fields of a `DataType`.
"""
nfields

"""
    show(stream, mime, x)

The `display` functions ultimately call `show` in order to write an object `x` as a
given `mime` type to a given I/O `stream` (usually a memory buffer), if possible. In order
to provide a rich multimedia representation of a user-defined type `T`, it is only necessary
to define a new `show` method for `T`, via: `show(stream, ::MIME"mime", x::T) = ...`,
where `mime` is a MIME-type string and the function body calls `write` (or similar) to write
that representation of `x` to `stream`. (Note that the `MIME""` notation only supports
literal strings; to construct `MIME` types in a more flexible manner use
`MIME{Symbol("")}`.)

For example, if you define a `MyImage` type and know how to write it to a PNG file, you
could define a function `show(stream, ::MIME"image/png", x::MyImage) = ...` to allow
your images to be displayed on any PNG-capable `Display` (such as IJulia). As usual, be sure
to `import Base.show` in order to add new methods to the built-in Julia function
`show`.

The default MIME type is `MIME"text/plain"`. There is a fallback definition for `text/plain`
output that calls `show` with 2 arguments. Therefore, this case should be handled by
defining a 2-argument `show(stream::IO, x::MyType)` method.

Technically, the `MIME"mime"` macro defines a singleton type for the given `mime` string,
which allows us to exploit Julia's dispatch mechanisms in determining how to display objects
of any given type.

The first argument to `show` can be an `IOContext` specifying output format properties.
See `IOContext` for details.
"""
show(stream, mime, x)

"""
    mean!(r, v)

Compute the mean of `v` over the singleton dimensions of `r`, and write results to `r`.
"""
mean!

"""
    join(strings, delim, [last])

Join an array of `strings` into a single string, inserting the given delimiter between
adjacent strings. If `last` is given, it will be used instead of `delim` between the last
two strings. For example

    join(["apples", "bananas", "pineapples"], ", ", " and ") == "apples, bananas and pineapples"

`strings` can be any iterable over elements `x` which are convertible to strings via `print(io::IOBuffer, x)`.
"""
join(strings, delim, last)

"""
    isless(x, y)

Test whether `x` is less than `y`, according to a canonical total order. Values that are
normally unordered, such as `NaN`, are ordered in an arbitrary but consistent fashion. This
is the default comparison used by `sort`. Non-numeric types with a canonical total order
should implement this function. Numeric types only need to implement it if they have special
values such as `NaN`.
"""
isless

"""
    expm1(x)

Accurately compute ``e^x-1``.
"""
expm1

"""
    showerror(io, e)

Show a descriptive representation of an exception object.
"""
showerror

"""
    error(message::AbstractString)

Raise an `ErrorException` with the given message.
"""
error

"""
    less(file::AbstractString, [line])

Show a file using the default pager, optionally providing a starting line number. Returns to
the `julia` prompt when you quit the pager.
"""
less(f::AbstractString, ?)

"""
    less(function, [types])

Show the definition of a function using the default pager, optionally specifying a tuple of
types to indicate which method to see.
"""
less(func, ?)

"""
    sqrtm(A)

If `A` has no negative real eigenvalues, compute the principal matrix square root of `A`,
that is the unique matrix ``X`` with eigenvalues having positive real part such that
``X^2 = A``. Otherwise, a nonprincipal square root is returned.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigfact`](:func:`eigfact`)) is
used to compute the square root. Otherwise, the square root is determined by means of the
Björck-Hammarling method, which computes the complex Schur form ([`schur`](:func:`schur`))
and then the complex square root of the triangular factor.

[^BH83]: Åke Björck and Sven Hammarling, "A Schur method for the square root of a matrix", Linear Algebra and its Applications, 52-53, 1983, 127-140. [doi:10.1016/0024-3795(83)80010-X](http://dx.doi.org/10.1016/0024-3795(83)80010-X)

"""
sqrtm

"""
    conv(u,v)

Convolution of two vectors. Uses FFT algorithm.
"""
conv

"""
    unsafe_store!(p::Ptr{T}, x, [i::Integer=1])

Store a value of type `T` to the address of the ith element (1-indexed) starting at `p`.
This is equivalent to the C expression `p[i-1] = x`.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Incorrect usage may corrupt or segfault your
program, in the same manner as C.
"""
unsafe_store!

"""
    expm(A)

Compute the matrix exponential of `A`, defined by

```math
e^A = \\sum_{n=0}^{\\infty} \\frac{A^n}{n!}.
```

For symmetric or Hermitian `A`, an eigendecomposition ([`eigfact`](:func:`eigfact`)) is
used, otherwise the scaling and squaring algorithm (see [^H05]) is chosen.

[^H05]: Nicholas J. Higham, "The squaring and scaling method for the matrix exponential revisited", SIAM Journal on Matrix Analysis and Applications, 26(4), 2005, 1179-1193. [doi:10.1137/090768539](http://dx.doi.org/10.1137/090768539)

"""
expm

"""
    hessfact!(A)

`hessfact!` is the same as [`hessfact`](:func:`hessfact`), but saves space by overwriting
the input `A`, instead of creating a copy.
"""
hessfact!

"""
    readcsv(source, [T::Type]; options...)

Equivalent to `readdlm` with `delim` set to comma.
"""
readcsv

"""
    erfcx(x)

Compute the scaled complementary error function of `x`, defined by ``e^{x^2} \\operatorname{erfc}(x)``.
Note also that ``\\operatorname{erfcx}(-ix)`` computes the Faddeeva function ``w(x)``.
"""
erfcx

"""
    UndefVarError(var::Symbol)

A symbol in the current scope is not defined.
"""
UndefVarError

"""
    gc()

Perform garbage collection. This should not generally be used.
"""
gc

"""
    iscntrl(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is a control character, or whether this is true for all elements
of a string. Control characters are the non-printing characters of the Latin-1 subset of Unicode.
"""
iscntrl

"""
    minimum!(r, A)

Compute the minimum value of `A` over the singleton dimensions of `r`, and write results to `r`.
"""
minimum!

"""
    .-(x, y)

Element-wise subtraction operator.
"""
Base.:(.-)

"""
    unsafe_trunc(T, x)

`unsafe_trunc(T, x)` returns the nearest integral value of type `T` whose absolute value is
less than or equal to `x`. If the value is not representable by `T`, an arbitrary value will
be returned.
"""
unsafe_trunc

"""
    parent(A)

Returns the "parent array" of an array view type (e.g., `SubArray`), or the array itself if
it is not a view.
"""
parent

"""
    nextpow(a, x)

The smallest `a^n` not less than `x`, where `n` is a non-negative integer. `a` must be
greater than 1, and `x` must be greater than 0.
"""
nextpow

"""
    gc_enable(on::Bool)

Control whether garbage collection is enabled using a boolean argument (`true` for enabled,
`false` for disabled). Returns previous GC state. Disabling garbage collection should be
used only with extreme caution, as it can cause memory use to grow without bound.
"""
gc_enable

"""
    readline(stream::IO=STDIN)
    readline(filename::AbstractString)

Read a single line of text, including a trailing newline character (if one is reached before
the end of the input), from the given I/O stream or file (defaults to `STDIN`).
When reading from a file, the text is assumed to be encoded in UTF-8.
"""
readline

"""
    atan(x)

Compute the inverse tangent of `x`, where the output is in radians.
"""
atan

"""
    joinpath(parts...) -> AbstractString

Join path components into a full path. If some argument is an absolute path, then prior
components are dropped.
"""
joinpath

"""
    homedir() -> AbstractString

Return the current user's home directory.
"""
homedir

"""
    isinf(f) -> Bool

Test whether a number is infinite.
"""
isinf

"""
    secd(x)

Compute the secant of `x`, where `x` is in degrees.
"""
secd

"""
    OverflowError()

The result of an expression is too large for the specified type and will cause a wraparound.
"""
OverflowError

"""
    redirect_stderr([stream])

Like `redirect_stdout`, but for `STDERR`.
"""
redirect_stderr

"""
    object_id(x)

Get a hash value for `x` based on object identity. `object_id(x)==object_id(y)` if `x === y`.
"""
object_id

"""
    unescape_string(io, s::AbstractString)

General unescaping of traditional C and Unicode escape sequences. Reverse of [`escape_string`](:func:`escape_string`).
"""
unescape_string(io, s)

"""
    digits!(array, n, [base])

Fills an array of the digits of `n` in the given base. More significant digits are at higher
indexes. If the array length is insufficient, the least significant digits are filled up to
the array length. If the array length is excessive, the excess portion is filled with zeros.
"""
digits!

"""
    cat(dims, A...)

Concatenate the input arrays along the specified dimensions in the iterable `dims`. For
dimensions not in `dims`, all input arrays should have the same size, which will also be the
size of the output array along that dimension. For dimensions in `dims`, the size of the
output array is the sum of the sizes of the input arrays along that dimension. If `dims` is
a single number, the different arrays are tightly stacked along that dimension. If `dims` is
an iterable containing several dimensions, this allows one to construct block diagonal
matrices and their higher-dimensional analogues by simultaneously increasing several
dimensions for every new input array and putting zero blocks elsewhere. For example,
`cat([1,2], matrices...)` builds a block diagonal matrix, i.e. a block matrix with
`matrices[1]`, `matrices[2]`, ... as diagonal blocks and matching zero blocks away from the
diagonal.
"""
cat

"""
    isupper(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is an uppercase letter, or whether this is true for all elements
of a string. A character is classified as uppercase if it belongs to Unicode category Lu,
Letter: Uppercase, or Lt, Letter: Titlecase.
"""
isupper

"""
    show(x)

Write an informative text representation of a value to the current output stream. New types
should overload `show(io, x)` where the first argument is a stream. The representation used
by `show` generally includes Julia-specific formatting and type information.
"""
show(x)

"""
    Array(dims)

`Array{T}(dims)` constructs an uninitialized dense array with element type `T`. `dims` may
be a tuple or a series of integer arguments. The syntax `Array(T, dims)` is also available,
but deprecated.
"""
Array

"""
    isreal(x) -> Bool

Test whether `x` or all its elements are numerically equal to some real number.
"""
isreal

"""
    issubtype(type1, type2)

Return `true` if and only if all values of `type1` are also of `type2`. Can also be written
using the `<:` infix operator as `type1 <: type2`.
"""
issubtype(type1, type2)

"""
    finalizer(x, function)

Register a function `f(x)` to be called when there are no program-accessible references to
`x`. The behavior of this function is unpredictable if `x` is of a bits type.
"""
finalizer

"""
    csch(x)

Compute the hyperbolic cosecant of `x`.
"""
csch

"""
    sec(x)

Compute the secant of `x`, where `x` is in radians.
"""
sec

"""
    recv(socket::UDPSocket)

Read a UDP packet from the specified socket, and return the bytes received. This call blocks.
"""
recv

"""
    TypeError(func::Symbol, context::AbstractString, expected::Type, got)

A type assertion failure, or calling an intrinsic function with an incorrect argument type.
"""
TypeError

"""
    pwd() -> AbstractString

Get the current working directory.
"""
pwd

"""
    getipaddr() -> IPAddr

Get the IP address of the local machine.
"""
getipaddr

"""
    uppercase(string)

Returns `string` with all characters converted to uppercase.
"""
uppercase

"""
    operm(file)

Like uperm but gets the permissions for people who neither own the file nor are a member of
the group owning the file
"""
operm

"""
    rpad(string, n, p)

Make a string at least `n` columns wide when printed, by padding on the right with copies of `p`.
"""
rpad

"""
    setfield!(value, name::Symbol, x)

Assign `x` to a named field in `value` of composite type. The syntax `a.b = c` calls
`setfield!(a, :b, c)`.
"""
setfield!

"""
    @printf([io::IOStream], "%Fmt", args...)

Print `args` using C `printf()` style format specification string. Optionally, an `IOStream`
may be passed as the first argument to redirect output.
"""
:@printf

"""
    rstrip(string, [chars])

Return `string` with any trailing whitespace removed. If `chars` (a character, or vector or
set of characters) is provided, instead remove characters contained in it.
"""
rstrip

"""
    countlines(io,[eol::Char])

Read `io` until the end of the stream/file and count the number of lines. To specify a file
pass the filename as the first argument. EOL markers other than '\\n' are supported by
passing them as the second argument.
"""
countlines

"""
```
*(A, B)
```

Matrix multiplication.
"""
Base.:(*)(::AbstractMatrix, ::AbstractMatrix)

"""
    .\\(x, y)

Element-wise left division operator.
"""
Base.:(.\)(x,y)

"""
```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.
"""
Base.:(*)(x, y...)

"""
```
*(s, t)
```

Concatenate strings. The `*` operator is an alias to this function.

```jldoctest
julia> "Hello " * "world"
"Hello world"
```
"""
Base.:(*)(s::AbstractString, t::AbstractString)

"""
    time()

Get the system time in seconds since the epoch, with fairly high (typically, microsecond) resolution.
"""
time()

"""
    procs(S::SharedArray)

Get the vector of processes that have mapped the shared array.
"""
procs(::SharedArray)

"""
    qr(A [,pivot=Val{false}][;thin=true]) -> Q, R, [p]

Compute the (pivoted) QR factorization of `A` such that either `A = Q*R` or `A[:,p] = Q*R`.
Also see `qrfact`. The default is to compute a thin factorization. Note that `R` is not
extended with zeros when the full `Q` is requested.
"""
qr

"""
    TextDisplay(stream)

Returns a `TextDisplay <: Display`, which can display any object as the text/plain MIME type
(only), writing the text representation to the given I/O stream. (The text representation is
the same as the way an object is printed in the Julia REPL.)
"""
TextDisplay

"""
    ismatch(r::Regex, s::AbstractString) -> Bool

Test whether a string contains a match of the given regular expression.
"""
ismatch

"""
    exp(x)

Compute ``e^x``.
"""
exp

"""
    searchindex(string, substring, [start])

Similar to `search`, but return only the start index at which the substring is found, or `0` if it is not.
"""
searchindex

"""
    listenany(port_hint) -> (UInt16,TCPServer)

Create a `TCPServer` on any port, using hint as a starting point. Returns a tuple of the
actual port that the server was created on and the server itself.
"""
listenany

"""
    getpid() -> Int32

Get Julia's process ID.
"""
getpid

"""
    matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

Return a vector of the matching substrings from eachmatch.
"""
matchall

"""
    get!(collection, key, default)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => default`, and return `default`.
"""
get!(collection,key,default)

"""
    get!(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => f()`, and return `f()`.

This is intended to be called using `do` block syntax:

    get!(dict, key) do
        # default value calculated here
        time()
    end
"""
get!(f::Function,collection,key)

"""
    @assert cond [text]

Throw an `AssertionError` if `cond` is `false`. Preferred syntax for writing assertions.
Message `text` is optionally displayed upon assertion failure.
"""
:@assert

"""
    listen([addr,]port) -> TCPServer

Listen on port on the address specified by `addr`. By default this listens on localhost
only. To listen on all interfaces pass `IPv4(0)` or `IPv6(0)` as appropriate.
"""
listen(addr,port)

"""
    listen(path) -> PipeServer

Create and listen on a named pipe / UNIX domain socket.
"""
listen(path)

"""
    deserialize(stream)

Read a value written by `serialize`. `deserialize` assumes the binary data read from
`stream` is correct and has been serialized by a compatible implementation of `serialize`.
It has been designed with simplicity and performance as a goal and does not validate
the data read. Malformed data can result in process termination. The caller has to ensure
the integrity and correctness of data read from `stream`.
"""
deserialize

"""
    ismarked(s)

Returns `true` if stream `s` is marked.

See also [`mark`](:func:`mark`), [`unmark`](:func:`unmark`), [`reset`](:func:`reset`).
"""
ismarked

"""
    first(coll)

Get the first element of an iterable collection. Returns the start point of a
[`Range`](:obj:`Range`) even if it is empty.
"""
first

"""
    median!(v)

Like `median`, but may overwrite the input vector.
"""
median!

"""
    cumprod!(B, A, [dim])

Cumulative product of `A` along a dimension, storing the result in `B`. The dimension defaults to 1.
"""
cumprod!

"""
    rethrow([e])

Throw an object without changing the current exception backtrace. The default argument is
the current exception (if called within a `catch` block).
"""
rethrow

"""
    reprmime(mime, x)

Returns an `AbstractString` or `Vector{UInt8}` containing the representation of `x` in the
requested `mime` type, as written by `show` (throwing a `MethodError` if no appropriate
`show` is available). An `AbstractString` is returned for MIME types with textual
representations (such as `"text/html"` or `"application/postscript"`), whereas binary data
is returned as `Vector{UInt8}`. (The function `istextmime(mime)` returns whether or not Julia
treats a given `mime` type as text.)

As a special case, if `x` is an `AbstractString` (for textual MIME types) or a
`Vector{UInt8}` (for binary MIME types), the `reprmime` function assumes that `x` is already
in the requested `mime` format and simply returns `x`.
"""
reprmime

"""
    rm(path::AbstractString; force=false, recursive=false)

Delete the file, link, or empty directory at the given path. If `force=true` is passed, a
non-existing path is not treated as error. If `recursive=true` is passed and the path is a
directory, then all contents are removed recursively.
"""
rm

"""
    graphemes(s) -> iterator over substrings of s

Returns an iterator over substrings of `s` that correspond to the extended graphemes in the
string, as defined by Unicode UAX #29. (Roughly, these are what users would perceive as
single characters, even though they may contain more than one codepoint; for example a
letter combined with an accent mark is a single grapheme.)
"""
graphemes

"""
    @__FILE__ -> AbstractString

`@__FILE__` expands to a string with the absolute file path of the file containing the
macro. Returns `nothing` if run from a REPL or an empty string if evaluated by
`julia -e <expr>`. Alternatively see [`PROGRAM_FILE`](:data:`PROGRAM_FILE`).
"""
:@__FILE__

"""
    charwidth(c)

Gives the number of columns needed to print a character.
"""
charwidth

"""
    abspath(path::AbstractString) -> AbstractString

Convert a path to an absolute path by adding the current directory if necessary.
"""
abspath

"""
    ispunct(c::Union{Char,AbstractString}) -> Bool

Tests whether a character belongs to the Unicode general category Punctuation, i.e. a
character whose category code begins with 'P'. For strings, tests whether this is true for
all elements of the string.
"""
ispunct

"""
    ismount(path) -> Bool

Returns `true` if `path` is a mount point, `false` otherwise.
"""
ismount

"""
    endswith(string, suffix)

Returns `true` if `string` ends with `suffix`. If `suffix` is a vector or set of
characters, tests whether the last character of `string` belongs to that set.
"""
endswith

"""
    !(x)

Boolean not.
"""
Base.:(!)

"""
    length(collection) -> Integer

For ordered, indexable collections, the maximum index `i` for which `getindex(collection, i)`
is valid. For unordered collections, the number of elements.
"""
length(collection)

"""
    length(s)

The number of characters in string `s`.
"""
length(::AbstractString)

"""
    bkfact(A) -> BunchKaufman

Compute the Bunch-Kaufman [^Bunch1977] factorization of a real symmetric or complex Hermitian
matrix `A` and return a `BunchKaufman` object. The following functions are available for
`BunchKaufman` objects: `size`, `\\`, `inv`, `issymmetric`, `ishermitian`.

[^Bunch1977]: J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. [url](http://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0).

"""
bkfact

"""
    searchsortedlast(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

Returns the index of the last value in `a` less than or equal to `x`, according to the
specified order. Returns `0` if `x` is less than all values in `a`.
"""
searchsortedlast

"""
    InterruptException()

The process was stopped by a terminal interrupt (CTRL+C).
"""
InterruptException

"""
    den(x)

Denominator of the rational representation of `x`.
"""
den

"""
    issubnormal(f) -> Bool

Test whether a floating point number is subnormal.
"""
issubnormal

"""
    NullException()

An attempted access to a `Nullable` with no defined value.
"""
NullException

"""
    .==(x, y)

Element-wise equality comparison operator.
"""
Base.:(.==)

"""
    cfunction(function::Function, ReturnType::Type, (ArgumentTypes...))

Generate C-callable function pointer from Julia function. Type annotation of the return
value in the callback function is a must for situations where Julia cannot infer the return
type automatically.

For example:

    function foo()
        # body

        retval::Float64
    end

    bar = cfunction(foo, Float64, ())
"""
cfunction

"""
    recvfrom(socket::UDPSocket) -> (address, data)

Read a UDP packet from the specified socket, returning a tuple of (address, data), where
address will be either IPv4 or IPv6 as appropriate.
"""
recvfrom

"""
    intersect(s1,s2...)
    ∩(s1,s2)

Construct the intersection of two or more sets. Maintains order and multiplicity of the
first argument for arrays and ranges.
"""
intersect

"""
    @spawn

Creates a closure around an expression and runs it on an automatically-chosen process,
returning a `Future` to the result.
"""
:@spawn

"""
    promote_rule(type1, type2)

Specifies what type should be used by `promote` when given values of types `type1` and
`type2`. This function should not be called directly, but should have definitions added to
it for new types as appropriate.
"""
promote_rule

"""
    mtime(file)

Equivalent to `stat(file).mtime`.
"""
mtime

"""
    sumabs2(itr)

Sum squared absolute values of all elements in a collection. This is equivalent to `sum(abs2(itr))` but faster.
"""
sumabs2(itr)

"""
    sumabs2(A, dims)

Sum squared absolute values of elements of an array over the given dimensions.
"""
sumabs2(A,dims)

"""
    uperm(file)

Gets the permissions of the owner of the file as a bitfield of

| Value | Description        |
|:------|:-------------------|
| 01    | Execute Permission |
| 02    | Write Permission   |
| 04    | Read Permission    |

For allowed arguments, see `stat`.
"""
uperm

"""
    showall(x)

Similar to `show`, except shows all elements of arrays.
"""
showall

"""
    mimewritable(mime, x)

Returns a boolean value indicating whether or not the object `x` can be written as the given
`mime` type. (By default, this is determined automatically by the existence of the
corresponding `show` function for `typeof(x)`.)
"""
mimewritable

"""
    match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

Search for the first match of the regular expression `r` in `s` and return a `RegexMatch`
object containing the match, or nothing if the match failed. The matching substring can be
retrieved by accessing `m.match` and the captured sequences can be retrieved by accessing
`m.captures` The optional `idx` argument specifies an index at which to start the search.
"""
match

"""
    qrfact!(A [,pivot=Val{false}])

`qrfact!` is the same as [`qrfact`](:func:`qrfact`) when `A` is a subtype of
`StridedMatrix`, but saves space by overwriting the input `A`, instead of creating a copy.
An `InexactError` exception is thrown if the factorisation produces a number not
representable by the element type of `A`, e.g. for integer types.
"""
qrfact!

"""
    coth(x)

Compute the hyperbolic cotangent of `x`.
"""
coth

"""
    start(iter) -> state

Get initial iteration state for an iterable object.
"""
start

"""
    relpath(path::AbstractString, startpath::AbstractString = ".") -> AbstractString

Return a relative filepath to path either from the current directory or from an optional
start directory. This is a path computation: the filesystem is not accessed to confirm the
existence or nature of path or startpath.
"""
relpath

"""
    readavailable(stream)

Read all available data on the stream, blocking the task only if no data is available. The
result is a `Vector{UInt8,1}`.
"""
readavailable

"""
    isa(x, type) -> Bool

Determine whether `x` is of the given `type`.
"""
isa

"""
    unsafe_load(p::Ptr{T}, [i::Integer=1])

Load a value of type `T` from the address of the ith element (1-indexed) starting at `p`.
This is equivalent to the C expression `p[i-1]`.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Incorrect usage may segfault your program or return
garbage answers, in the same manner as C.
"""
unsafe_load

"""
    catch_backtrace()

Get the backtrace of the current exception, for use within `catch` blocks.
"""
catch_backtrace

"""
    cos(x)

Compute cosine of `x`, where `x` is in radians.
"""
cos

"""
    base64encode(writefunc, args...)
    base64encode(args...)

Given a `write`-like function `writefunc`, which takes an I/O stream as its first argument,
`base64encode(writefunc, args...)` calls `writefunc` to write `args...` to a base64-encoded
string, and returns the string. `base64encode(args...)` is equivalent to `base64encode(write, args...)`:
it converts its arguments into bytes using the standard `write` functions and returns the
base64-encoded string.
"""
base64encode

"""
    filt!(out, b, a, x, [si])

Same as [`filt`](:func:`filt`) but writes the result into the `out` argument, which may
alias the input `x` to modify it in-place.
"""
filt!

"""
    ascii(s::AbstractString)

Convert a string to `String` type and check that it contains only ASCII data, otherwise
throwing an `ArgumentError` indicating the position of the first non-ASCII byte.
"""
ascii(s)

"""
    maxabs(A, dims)

Compute the maximum absolute values over given dimensions.
"""
maxabs(A,dims)

"""
    done(iter, state) -> Bool

Test whether we are done iterating.
"""
done

"""
    convert(T, x)

Convert `x` to a value of type `T`.

If `T` is an `Integer` type, an [`InexactError`](:exc:`InexactError`) will be raised if `x`
is not representable by `T`, for example if `x` is not integer-valued, or is outside the
range supported by `T`.

```jldoctest
julia> convert(Int, 3.0)
3

julia> convert(Int, 3.5)
ERROR: InexactError()
 in convert(::Type{Int64}, ::Float64) at ./int.jl:330
 ...
```

If `T` is a [`AbstractFloat`](:obj:`AbstractFloat`) or [`Rational`](:obj:`Rational`) type,
then it will return the closest value to `x` representable by `T`.

```jldoctest
julia> x = 1/3
0.3333333333333333

julia> convert(Float32, x)
0.33333334f0

julia> convert(Rational{Int32}, x)
1//3

julia> convert(Rational{Int64}, x)
6004799503160661//18014398509481984
```

If `T` is a collection type and `x` a collection, the result of `convert(T, x)` may alias
`x`.
```jldoctest
julia> x = Int[1,2,3];

julia> y = convert(Vector{Int}, x);

julia> y === x
true
```
Similarly, if `T` is a composite type and `x` a related instance, the result of
`convert(T, x)` may alias part or all of `x`.
```jldoctest
julia> x = speye(5);

julia> typeof(x)
SparseMatrixCSC{Float64,Int64}

julia> y = convert(SparseMatrixCSC{Float64,Int64}, x);

julia> z = convert(SparseMatrixCSC{Float32,Int64}, y);

julia> y === x
true

julia> z === x
false

julia> z.colptr === x.colptr
true
```
"""
convert

"""
    applicable(f, args...) -> Bool

Determine whether the given generic function has a method applicable to the given arguments.

```jldoctest
julia> function f(x, y)
           x + y
       end;

julia> applicable(f, 1)
false

julia> applicable(f, 1, 2)
true
```
"""
applicable

"""
    fma(x, y, z)

Computes `x*y+z` without rounding the intermediate result `x*y`. On some systems this is
significantly more expensive than `x*y+z`. `fma` is used to improve accuracy in certain
algorithms. See [`muladd`](:func:`muladd`).
"""
fma

"""

    eigvals(A,[irange,][vl,][vu]) -> values

Returns the eigenvalues of `A`. If `A` is `Symmetric`, `Hermitian` or `SymTridiagonal`,
it is possible to calculate only a subset of the eigenvalues by specifying either a
`UnitRange` `irange` covering indices of the sorted eigenvalues, or a pair `vl` and `vu`
for the lower and upper boundaries of the eigenvalues.

For general non-symmetric matrices it is possible to specify how the matrix is balanced
before the eigenvector calculation. The option `permute=true` permutes the matrix to
become closer to upper triangular, and `scale=true` scales the matrix by its diagonal
elements to make rows and columns moreequal in norm. The default is `true` for both
options.
"""
eigvals

"""
    escape_string(str::AbstractString) -> AbstractString

General escaping of traditional C and Unicode escape sequences.
"""
escape_string(str)

"""
    pointer_from_objref(object_instance)

Get the memory address of a Julia object as a `Ptr`. The existence of the resulting `Ptr`
will not protect the object from garbage collection, so you must ensure that the object
remains referenced for the whole time that the `Ptr` will be used.
"""
pointer_from_objref

"""
    copy!(dest, src)

Copy all elements from collection `src` to array `dest`. Returns `dest`.
"""
copy!(dest,src)

"""
    copy!(dest, do, src, so, N)

Copy `N` elements from collection `src` starting at offset `so`, to array `dest` starting at
offset `do`. Returns `dest`.
"""
copy!(dest,d,src,so,N)

"""
    ntoh(x)

Converts the endianness of a value from Network byte order (big-endian) to that used by the Host.
"""
ntoh

"""
    qrfact(A) -> SPQR.Factorization

Compute the QR factorization of a sparse matrix `A`. A fill-reducing permutation is used.
The main application of this type is to solve least squares problems with `\\`. The function
calls the C library SPQR and a few additional functions from the library are wrapped but not
exported.
"""
qrfact(A)

"""
    +(x, y...)

Addition operator. `x+y+z+...` calls this function with all arguments, i.e. `+(x, y, z, ...)`.
"""
+

"""
    setindex!(A, X, inds...)

Store values from array `X` within some subset of `A` as specified by `inds`.
"""
setindex!(A::AbstractArray,X,inds...)

"""
    setindex!(collection, value, key...)

Store the given value at the given key or index within a collection. The syntax `a[i,j,...] =
x` is converted by the compiler to `(setindex!(a, x, i, j, ...); x)`.
"""
setindex!(collection,value,key...)

"""
    signif(x, digits, [base])

Rounds (in the sense of `round`) `x` so that there are `digits` significant digits, under a
base `base` representation, default 10. E.g., `signif(123.456, 2)` is `120.0`, and
`signif(357.913, 4, 2)` is `352.0`.
"""
signif

"""
    full(F)

Reconstruct the matrix `A` from the factorization `F=factorize(A)`.
"""
full(F)

"""
    full(QRCompactWYQ[, thin=true]) -> Matrix

Converts an orthogonal or unitary matrix stored as a `QRCompactWYQ` object, i.e. in the
compact WY format [^Bischof1987], to a dense matrix.

Optionally takes a `thin` Boolean argument, which if `true` omits the columns that span the
rows of `R` in the QR factorization that are zero. The resulting matrix is the `Q` in a thin
QR factorization (sometimes called the reduced QR factorization). If `false`, returns a `Q`
that spans all rows of `R` in its corresponding QR factorization.
"""
full(::LinAlg.QRCompactWYQ, ?)

"""
    throw(e)

Throw an object as an exception.
"""
throw

"""
    isxdigit(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is a valid hexadecimal digit, or whether this is true for all elements of a string.
"""
isxdigit

"""
    issubset(a, b)
    ⊆(a,b) -> Bool
    ⊈(a,b) -> Bool
    ⊊(a,b) -> Bool

Determine whether every element of `a` is also in `b`, using [`in`](:func:`in`).
"""
issubset(a,b)

"""
    issubset(A, S) -> Bool
    ⊆(A,S) -> Bool

Return `true` if `A` is a subset of or equal to `S`.
"""
issubset

"""
    unescape_string(s::AbstractString) -> AbstractString

General unescaping of traditional C and Unicode escape sequences. Reverse of
[`escape_string`](:func:`escape_string`). See also [`unescape_string`](:func:`unescape_string`).
"""
unescape_string(s)

"""
    redirect_stdout()

Create a pipe to which all C and Julia level `STDOUT` output will be redirected. Returns a
tuple `(rd,wr)` representing the pipe ends. Data written to `STDOUT` may now be read from
the rd end of the pipe. The wr end is given for convenience in case the old `STDOUT` object
was cached by the user and needs to be replaced elsewhere.
"""
redirect_stdout

"""
    redirect_stdout(stream)

Replace `STDOUT` by stream for all C and Julia level output to `STDOUT`. Note that `stream`
must be a TTY, a `Pipe` or a `TCPSocket`.
"""
redirect_stdout(stream)

"""
    print_with_color(color::Symbol, [io], strings...)

Print strings in a color specified as a symbol.

`color` may take any of the values $(Base.available_text_colors_docstring).
"""
print_with_color

"""
    stringmime(mime, x)

Returns an `AbstractString` containing the representation of `x` in the requested `mime`
type. This is similar to `reprmime` except that binary data is base64-encoded as an ASCII string.
"""
stringmime

"""
    ischardev(path) -> Bool

Returns `true` if `path` is a character device, `false` otherwise.
"""
ischardev

"""
    zero(x)

Get the additive identity element for the type of `x` (`x` can also specify the type itself).
"""
zero

"""
    any(A, dims)

Test whether any values along the given dimensions of an array are `true`.
"""
any(::AbstractArray,dims)

"""
    zeros(type, dims)

Create an array of all zeros of specified type. The type defaults to Float64 if not specified.
"""
zeros(t,dims)

"""
    zeros(A)

Create an array of all zeros with the same element type and shape as `A`.
"""
zeros(A)

"""
    Symbol(x...) -> Symbol

Create a `Symbol` by concatenating the string representations of the arguments together.
"""
Symbol

"""
    zeta(s)

Riemann zeta function ``\\zeta(s)``.
"""
zeta(s)

"""
    isvalid(value) -> Bool

Returns `true` if the given value is valid for its type, which currently can be either
`Char` or `String`.
"""
isvalid(value)

"""
    isvalid(T, value) -> Bool

Returns `true` if the given value is valid for that type. Types currently can
be either `Char` or `String`. Values for `Char` can be of type `Char` or `UInt32`.
Values for `String` can be of that type, or `Vector{UInt8}`.
"""
isvalid(T,value)

"""
    isvalid(str, i)

Tells whether index `i` is valid for the given string.
"""
isvalid(::AbstractString,i)

"""
    rsplit(string, [chars]; limit=0, keep=true)

Similar to `split`, but starting from the end of the string.
"""
rsplit

"""
    runtests([tests=["all"] [, numcores=ceil(Integer, Sys.CPU_CORES / 2) ]])

Run the Julia unit tests listed in `tests`, which can be either a string or an array of
strings, using `numcores` processors. (not exported)
"""
runtests

"""
    time_ns()

Get the time in nanoseconds. The time corresponding to 0 is undefined, and wraps every 5.8 years.
"""
time_ns

"""
    rsearchindex(string, substring, [start])

Similar to `rsearch`, but return only the start index at which the substring is found, or `0` if it is not.
"""
rsearchindex

"""
    unsigned(x) -> Unsigned

Convert a number to an unsigned integer. If the argument is signed, it is reinterpreted as
unsigned without checking for negative values.
"""
unsigned

"""
    mkdir(path, [mode])

Make a new directory with name `path` and permissions `mode`. `mode` defaults to `0o777`,
modified by the current file creation mask.
"""
mkdir

"""
    midpoints(e)

Compute the midpoints of the bins with edges `e`. The result is a vector/range of length
`length(e) - 1`. Note: Julia does not ignore `NaN` values in the computation.
"""
midpoints

"""
    .+(x, y)

Element-wise addition operator.
"""
Base.:(.+)

"""
    reverseind(v, i)

Given an index `i` in `reverse(v)`, return the corresponding index in `v` so that
`v[reverseind(v,i)] == reverse(v)[i]`. (This can be nontrivial in the case where `v` is a
Unicode string.)
"""
reverseind

"""
    float(x)

Convert a number, array, or string to a `AbstractFloat` data type. For numeric data, the
smallest suitable `AbstractFloat` type is used. Converts strings to `Float64`.
"""
float

"""
    include_dependency(path::AbstractString)

In a module, declare that the file specified by `path` (relative or absolute) is a
dependency for precompilation; that is, the module will need to be recompiled if this file
changes.

This is only needed if your module depends on a file that is not used via `include`. It has
no effect outside of compilation.
"""
include_dependency

"""
    islower(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is a lowercase letter, or whether this is true for all elements of
a string. A character is classified as lowercase if it belongs to Unicode category Ll,
Letter: Lowercase.
"""
islower

"""
    signbit(x)

Returns `true` if the value of the sign of `x` is negative, otherwise `false`.
"""
signbit

"""
    cscd(x)

Compute the cosecant of `x`, where `x` is in degrees.
"""
cscd

"""
    tryparse(type, str, [base])

Like `parse`, but returns a `Nullable` of the requested type. The result will be null if the
string does not contain a valid number.
"""
tryparse

"""
    all!(r, A)

Test whether all values in `A` along the singleton dimensions of `r` are `true`, and write results to `r`.
"""
all!

"""
    is_assigned_char(c) -> Bool

Returns `true` if the given char or integer is an assigned Unicode code point.
"""
is_assigned_char

"""
    exit([code])

Quit (or control-D at the prompt). The default exit code is zero, indicating that the
processes completed successfully.
"""
exit

"""
    istextmime(m::MIME)

Determine whether a MIME type is text data.
"""
istextmime

"""
    realpath(path::AbstractString) -> AbstractString

Canonicalize a path by expanding symbolic links and removing "." and ".." entries.
"""
realpath

"""
    skipchars(stream, predicate; linecomment::Char)

Advance the stream until before the first character for which `predicate` returns `false`.
For example `skipchars(stream, isspace)` will skip all whitespace. If keyword argument
`linecomment` is specified, characters from that character through the end of a line will
also be skipped.
"""
skipchars

"""
    realmin(T)

The smallest in absolute value non-subnormal value representable by the given floating-point DataType `T`.
"""
realmin

"""
    union!(s, iterable)

Union each element of `iterable` into set `s` in-place.
"""
union!

"""
    deepcopy(x)

Create a deep copy of `x`: everything is copied recursively, resulting in a fully
independent object. For example, deep-copying an array produces a new array whose elements
are deep copies of the original elements. Calling `deepcopy` on an object should generally
have the same effect as serializing and then deserializing it.

As a special case, functions can only be actually deep-copied if they are anonymous,
otherwise they are just copied. The difference is only relevant in the case of closures,
i.e. functions which may contain hidden internal references.

While it isn't normally necessary, user-defined types can override the default `deepcopy`
behavior by defining a specialized version of the function `deepcopy_internal(x::T, dict::ObjectIdDict)`
(which shouldn't otherwise be used), where `T` is the type to be specialized for, and `dict`
keeps track of objects copied so far within the recursion. Within the definition,
`deepcopy_internal` should be used in place of `deepcopy`, and the `dict` variable should be
updated as appropriate before returning.
"""
deepcopy

"""
    widen(x)

If `x` is a type, return a "larger" type (for numeric types, this will be
a type with at least as much range and precision as the argument, and usually more).
Otherwise `x` is converted to `widen(typeof(x))`.

```jldoctest
julia> widen(Int32)
Int64

julia> widen(1.5f0)
1.5
```
"""
widen

"""
    Set([itr])

Construct a [`Set`](:obj:`Set`) of the values generated by the given iterable object, or an
empty set. Should be used instead of [`IntSet`](:obj:`IntSet`) for sparse integer sets, or
for sets of arbitrary objects.
"""
Set

"""
    erf(x)

Compute the error function of `x`, defined by ``\\frac{2}{\\sqrt{\\pi}} \\int_0^x e^{-t^2} dt``
for arbitrary complex `x`.
"""
erf

"""
    isprint(c::Union{Char,AbstractString}) -> Bool

Tests whether a character is printable, including spaces, but not a control character. For
strings, tests whether this is true for all elements of the string.
"""
isprint

"""
    splitdir(path::AbstractString) -> (AbstractString,AbstractString)

Split a path into a tuple of the directory name and file name.
"""
splitdir

"""
    signed(x)

Convert a number to a signed integer. If the argument is unsigned, it is reinterpreted as
signed without checking for overflow.
"""
signed

"""
    Val{c}

Create a "value type" out of `c`, which must be an `isbits` value. The intent of this
construct is to be able to dispatch on constants, e.g., `f(Val{false})` allows you to
dispatch directly (at compile-time) to an implementation `f(::Type{Val{false}})`, without
having to test the boolean value at runtime.
"""
Val

"""
    iswritable(io) -> Bool

Returns `true` if the specified IO object is writable (if that can be determined).
"""
iswritable

"""
    |(x, y)

Bitwise or.
"""
Base.:(|)

"""
    splitdrive(path::AbstractString) -> (AbstractString,AbstractString)

On Windows, split a path into the drive letter part and the path part. On Unix systems, the
first component is always the empty string.
"""
splitdrive

"""
    pop!(collection, key[, default])

Delete and return the mapping for `key` if it exists in `collection`, otherwise return
`default`, or throw an error if default is not specified.
"""
pop!(collection,key,?)

"""
    pop!(collection) -> item

Remove the last item in `collection` and return it.

```jldoctest
julia> A=[1, 2, 3, 4, 5, 6]
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6

julia> pop!(A)
6

julia> A
5-element Array{Int64,1}:
 1
 2
 3
 4
 5
```
"""
pop!(collection)

"""
    seekend(s)

Seek a stream to its end.
"""
seekend

"""
    DivideError()

Integer division was attempted with a denominator value of 0.
"""
DivideError

"""
    unsafe_pointer_to_objref(p::Ptr)

Convert a `Ptr` to an object reference. Assumes the pointer refers to a valid heap-allocated
Julia object. If this is not the case, undefined behavior results, hence this function is
considered "unsafe" and should be used with care.
"""
unsafe_pointer_to_objref

"""
    chomp(string)

Remove a single trailing newline from a string.
"""
chomp

"""
    dawson(x)

Compute the Dawson function (scaled imaginary error function) of `x`, defined by
``\\frac{\\sqrt{\\pi}}{2} e^{-x^2} \\operatorname{erfi}(x)``.
"""
dawson

"""
    \$(x, y)

Bitwise exclusive or.
"""
Base.:$(x, y)

"""
    getsockname(sock::Union{TCPServer, TCPSocket}) -> (IPAddr,UInt16)

Get the IP address and the port that the given TCP socket is connected to (or bound to, in the case of TCPServer).
"""
getsockname
