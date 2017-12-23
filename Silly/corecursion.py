from PIL import Image, ImageDraw, ImageFont
import math
import colorsys
# get an image
#base = Image.open('Pillow/Tests/images/lena.png').convert('RGBA')

r = 0.25
def pos(t, x, y):
    w = math.exp(r*t)
    return (w*x+1-w, w*y+1-w)

# make a blank image for the text, initial
def frac(x):
    return x-math.floor(x)

for k in range(100):
    (r1, g1, b1) = colorsys.hsv_to_rgb(k/100.0, 1.0, 1.0)
    (r2, g2, b2) = colorsys.hsv_to_rgb(frac(0.5+k/100.0), 1.0, 1.0)
    print (r2,g2,b2)
    txt = Image.new('RGBA', (512,512), (int(r1*255),int(g1*255),int(b1*255),255))
    t = 0.1*k
    sentence = "because proper recursion needs a base case this isn't recursion because proper recursion needs a base case so this isn't recursion because recursion"
    for i, word in enumerate(sentence.split()):
        x, y = pos(1+t-i, 0.25, 0.25)
        fnt = ImageFont.truetype('/usr/local/texlive/2017/texmf-dist/fonts/truetype/public/gnu-freefont/FreeSans.ttf', int(math.exp(r*(t-i))*60))
        print fnt
        d = ImageDraw.Draw(txt)

        d.text((512*x,512*y), word, font=fnt, fill=(int(r2*255),int(g2*255), int(b2*255),255))
    txt.save('%04d.png' % k, 'PNG')

#    out = txt

#    out.show()
∫
