#!/usr/bin/env python
# -*- coding: ISO-8859-1 -*-

#MD5 test suite:
#MD5 ("") = d41d8cd98f00b204e9800998ecf8427e
#MD5 ("a") = 0cc175b9c0f1b6a831c399e269772661
#MD5 ("abc") = 900150983cd24fb0d6963f7d28e17f72
#MD5 ("message digest") = f96b697d7cb7938d525a2f31aaf161d0
#MD5 ("abcdefghijklmnopqrstuvwxyz") = c3fcd3d76192e4007dfb496cca67e13b
#MD5 ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") = d174ab98d277d9f5a5611c2c9f419d9f
#MD5 ("12345678901234567890123456789012345678901234567890123456789012345678901234567890") = 57edf4a22be3c955ac49da2e2107b67a

#SHA test suite:
#SHA ("abc") = A9993E36 4706816A BA3E2571 7850C26C 9CD0D89D
#SHA ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") = 84983E44 1C3BD26E BAAE4AA1 F95129E5 E54670F1
#SHA (A million repetitions of "a") = 34AA973C D4C4DAA4 F61EEB2B DBAD2731 6534016F

import Tkinter
import ScrolledText
import tkFileDialog
import sys
import tkFont

from md5 import *
from sha import *

hashes = {"MD5" : md5, "SHA" : sha}
nbsp = list(" ")

def getHash(file, hashOp, delimiter) :

    try :
        reader = open(file, 'rb')
    except :
        return ""
    
    try :
        digest = hashOp()
        while True :
            chunk = reader.read(1024)
            if not chunk :
                break
            digest.update(chunk)
    finally:
        reader.close()
        
    raw = digest.hexdigest()
    work = []
    i = 0
    while i < len(raw) :
        work.append(raw[i])
        i += 1
        if 0 == (i%8) :
            work = work + delimiter

    return ''.join(work)


def doGetHashes() :
  filenames = tkFileDialog.askopenfilenames()
  updateCanvas(filenames)
  
def updateCanvas(filenames):
  textarea.config(state=Tkinter.NORMAL)
  for file in filenames :
    textarea.insert(Tkinter.END, ("%s\n" % file))

    for hashOp in hashes :
      anHash = getHash(file, hashes[hashOp], nbsp)
      textarea.insert(Tkinter.END, ("%s " % hashOp) )

      textarea.insert(Tkinter.END, ("%s\n" % anHash), 'mono' )

    textarea.insert(Tkinter.END, "-------------------------\n")
  textarea.tag_config('mono', font='courier')
  textarea.config(state=Tkinter.DISABLED)

if __name__ == "__main__":
  root = Tkinter.Tk()
  root.title("tkFileHasher")

  textarea = ScrolledText.ScrolledText()
  textarea.pack()
  textarea.config(state=Tkinter.DISABLED)
  Tkinter.Button(text="Select files to hash...", command=doGetHashes).pack()
  updateCanvas(sys.argv[1:])
  Tkinter.mainloop()