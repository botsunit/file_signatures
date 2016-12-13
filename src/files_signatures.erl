% @hidden
-module(files_signatures).

-compile(export_all).

png(<<16#89, 16#50, 16#4E, 16#47, 16#0D, 16#0A, 16#1A, 16#0A, _/binary>>) -> true;
png(_) -> false.

tiff(<<16#49, 16#49, 16#2A, 16#00, _/binary>>) -> true;
tiff(<<16#4D, 16#4D, 16#00, 16#2A, _/binary>>) -> true;
tiff(_) -> false.

gif(<<16#47, 16#49, 16#46, 16#38, 16#37, 16#61, _/binary>>) -> true;
gif(<<16#47, 16#49, 16#46, 16#38, 16#39, 16#61, _/binary>>) -> true;
gif(_) -> false.

jpeg(<<16#FF, 16#D8, 16#FF, 16#DB,
       _/binary>>) -> true;
jpeg(<<16#FF, 16#D8, 16#FF, 16#E0, _:1/binary, _:1/binary,
       16#4A, 16#46, 16#49, 16#46, 16#00,
       _/binary>>) -> true;
jpeg(<<16#FF, 16#D8, 16#FF, 16#E1, _:1/binary, _:1/binary,
       16#45, 16#78, 16#69, 16#66, 16#00,
       _/binary>>) -> true;
jpeg(<<16#FF, 16#D8, 16#FF, 16#E2,
       _/binary>>) -> true;
jpeg(<<16#FF, 16#D8, 16#FF, 16#E3,
       _/binary>>) -> true;
jpeg(<<16#FF, 16#D8, 16#FF, 16#FE,
       _/binary>>) -> true;
jpeg(<<16#FF, 16#D8, 16#FF, 16#E8, _:1/binary, _:1/binary,
       16#53, 16#50, 16#49, 16#46, 16#46, 16#00,
       _/binary>>) -> true;
jpeg(_) -> false.
jpg(Data) -> jpeg(Data).

zip(<<16#50, 16#4B, 16#03, 16#04, _/binary>>) -> true;
zip(<<16#50, 16#4B, 16#05, 16#06, _/binary>>) -> true;
zip(<<16#50, 16#4B, 16#07, 16#08, _/binary>>) -> true;
zip(_) -> false.
jar(Data) -> zip(Data).
odt(Data) -> zip(Data).
ods(Data) -> zip(Data).
odp(Data) -> zip(Data).
docx(Data) -> zip(Data).
xlsx(Data) -> zip(Data).
pptx(Data) -> zip(Data).
vsdx(Data) -> zip(Data).
apk(Data) -> zip(Data).

ps(<<16#25, 16#21, 16#50, 16#53, _/binary>>) -> true;
ps(_) -> false.

pdf(<<16#25, 16#50, 16#44, 16#46, _/binary>>) -> true;
pdf(_) -> false.

avi(<<16#52, 16#49, 16#46, 16#46,
      _:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#41, 16#56, 16#49, 16#20, _/binary>>) -> true;
avi(_) -> false.

mp3(<<16#FF, 16#FB, _/binary>>) -> true;
mp3(<<16#49, 16#44, 16#33, _/binary>>) -> true;
mp3(_) -> false.

bmp(<<16#42, 16#4D, _/binary>>) -> true;
bmp(_) -> false.
dib(Data) -> bmp(Data).

wmv(<<16#30, 16#26, 16#B2, 16#75, 16#8E, 16#66, 16#CF, 16#11,
      16#A6, 16#D9, 16#00, 16#AA, 16#00, 16#62, 16#CE, 16#6C, _/binary>>) -> true;
wmv(_) -> false.
wma(Data) -> wmv(Data).
asf(Data) -> wmv(Data).

webm(<<16#1A, 16#45, 16#DF, 16#A3, _/binary>>) -> true;
webm(_) -> false.
mkv(Data) -> webm(Data).
mka(Data) -> webm(Data).
mks(Data) -> webm(Data).
mk3d(Data) -> webm(Data).

ogg(<<16#4F, 16#67, 16#67, 16#53, _/binary>>) -> true;
ogg(_) -> false.
oga(Data) -> ogg(Data).
ogv(Data) -> ogg(Data).

'3gp'(<<16#00, 16#00, 16#00, 16#14, 16#66, 16#74,
        16#79, 16#70, _/binary>>) -> true;
'3gp'(<<16#00, 16#00, 16#00, 16#20, 16#66, 16#74,
        16#79, 16#70, _/binary>>) -> true;
'3gp'(_) -> false.
'3g2'(Data) -> '3gp'(Data).
'3gg'(Data) -> '3gp'(Data).

z(<<16#1F, 16#9D, _/binary>>) -> true;
z(<<16#1F, 16#A0, _/binary>>) -> true;
z(_) -> false.

bz2(<<16#42, 16#5A, 16#68, _/binary>>) -> true;
bz2(_) -> false.

ico(<<16#00, 16#00, 16#01, 16#00, _/binary>>) -> true;
ico(_) -> false.

aiff(<<16#46, 16#4F, 16#52, 16#4D,
       _:1/binary, _:1/binary, _:1/binary, _:1/binary,
       16#41, 16#49, 16#46, 16#46, _/binary>>) -> true;
aiff(_) -> false.
aif(Data) -> aiff(Data).
aifc(Data) -> aiff(Data).
snd(Data) -> aiff(Data).
iff(Data) -> aiff(Data).

rar(<<16#52, 16#61, 16#72, 16#21, 16#1A, 16#07, 16#00, _/binary>>) -> true;
rar(<<16#52, 16#61, 16#72, 16#21, 16#1A, 16#07, 16#01, 16#00, _/binary>>) -> true;
rar(_) -> false.

class(<<16#CA, 16#FE, 16#BA, 16#BE, _/binary>>) -> true;
class(_) -> false.

wav(<<16#52, 16#49, 16#46, 16#46,
      _:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#57, 16#41, 16#56, 16#45, _/binary>>) -> true;
wav(_) -> false.

mid(<<16#4D, 16#54, 16#68, 16#64, _/binary>>) -> true;
mid(_) -> false.
midi(Data) -> mid(Data).

tar(<<_:16#101/binary, 16#75, 16#73, 16#74, 16#61, 16#72, 16#00, 16#30, 16#30, _/binary>>) -> true;
tar(<<_:16#101/binary, 16#75, 16#73, 16#74, 16#61, 16#72, 16#20, 16#20, 16#00, _/binary>>) -> true;
tar(_) -> false.

'7z'(<<16#37, 16#7A, 16#BC, 16#AF, 16#27, 16#1C, _/binary>>) -> true;
'7z'(_) -> false.

gz(<<16#1F, 16#8B, _/binary>>) -> true;
gz(_) -> false.

lz4(<<16#04, 16#22, 16#4D, 16#18, _/binary>>) -> true;
lz4(_) -> false.

mp4(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#66, 16#74, 16#79, 16#70, 16#33, 16#67, 16#70, 16#35, _/binary>>) -> true;
mp4(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#66, 16#74, 16#79, 16#70, 16#4D, 16#53, 16#4E, 16#56, _/binary>>) -> true;
mp4(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#66, 16#74, 16#79, 16#70, 16#69, 16#73, 16#6F, 16#6D, _/binary>>) -> true;
mp4(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#66, 16#74, 16#79, 16#70, 16#6D, 16#70, 16#34, 16#32, _/binary>>) -> true;
mp4(_) -> false.

m4a(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#66, 16#74, 16#79, 16#70, 16#4D, 16#34, 16#41, 16#20, _/binary>>) -> true;
m4a(_) -> false.

m4v(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#66, 16#74, 16#79, 16#70, _/binary>>) -> true;
m4v(_) -> false.

mov(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#66, 16#74, 16#79, 16#70, 16#71, 16#74, 16#20, 16#20, _/binary>>) -> true;
mov(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#6D, 16#6F, 16#6F, 16#76, _/binary>>) -> true;
mov(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#66, 16#72, 16#65, 16#65, _/binary>>) -> true;
mov(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#6D, 16#64, 16#61, 16#74, _/binary>>) -> true;
mov(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#77, 16#69, 16#64, 16#65, _/binary>>) -> true;
mov(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#70, 16#6E, 16#6F, 16#74, _/binary>>) -> true;
mov(<<_:1/binary, _:1/binary, _:1/binary, _:1/binary,
      16#73, 16#6B, 16#69, 16#70, _/binary>>) -> true;
mov(_) -> false.

mpeg(<<16#00, 16#00, 16#01, 16#BA, _/binary>>) -> true;
mpeg(<<16#00, 16#00, 16#01, 16#B3, _/binary>>) -> true;
mpeg(_) -> false.
mpg(Data) -> mpeg(Data).

flv(<<16#46, 16#4C, 16#56, _/binary>>) -> true;
flv(_) -> false.

