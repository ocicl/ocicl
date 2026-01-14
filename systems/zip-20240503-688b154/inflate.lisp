;; inflate.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;
;; $Id: inflate.lisp,v 1.1.1.1 2005/04/03 19:36:28 dlichteblau Exp $

;; Description:
;;   inflate a stream of bytes which was compressed with the Deflate
;;   algorithm
;;
;;   john foderaro, August 2001
;;
;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



#|
Programming interface:

(inflate input-stream output-stream)
- the compressed information from the input-stream is read and 
  the uncompressed information is written to the output-stream
- both streams must support (unsigned-byte 8) element reading and writing


(skip-gzip-header input-stream)
- if the input stream is positioned on the header of a gzip'ed file
   then skip that header.
- if the input stream is not positioned on a gzip header then nothing 
  is done.

|#



#|
		The Deflate Compression Algorithm

reference: http://www.gzip.org/zlib/rfc-deflate.html

Basic idea:
Deflation is a means of compressing an octet sequence that
combines the LZ77 algorithm for marking common substrings and
Huffman coding to take advantage of different frequency of occurance
for each possible values in the file.
This algorithm may not be as easy to understand or as efficient
as the LZW compression algorithm but Deflate does have the big
advantage in that it is not patented.  Thus Deflate is a very
widely used.  Nowdays it's the most common compression method
used in Windows Zip programs (e.g. Winzip) and in the Unix gzip program.
Java jar files, being just zip files, also use this compression method.


Lempel-Ziv 1977 (LZ77):
An octet sequence often contains repeated subsequences.  The LZ algorithm
compresses a file by replacing repeated substrings with (Length,Distance)
markers which mean during decompression: Go back Distance octets 
in output stream and copy Length bytes to the output stream.  

Huffman Coding:
A Huffman code for a set of values V assigns a unique bitsequence
to each value in V.   A bitsequence is a sequence of 0's and 1'.
An important property of Huffman codes is that if X is a bitsequence
for a value in V then no other value in V has a bitsequence 
with X as a prefix of that sequence.  This means that if you see
the bitsequence X in the stream you know that this denotes the value
v and you don't have to read any more bits.


Blocks:
A deflated file is a sequence of blocks.  There are three types of
blocks:
1. uncompressed - The block simply contains the same sequence of 
octets as were found in the input stream.  This type of block
is useful when the input stream has already been compressed (e.g.
it's a jpg or gif file) as compressing a compressed file often
results in the file getting larger.

2. compressed with fixed Huffman code - The block contains a 
huffman-coded LZ77 compressed bitsequence.  The huffman code
used is specified by the deflate algorithm.   This type of block
is useful when the octet sequence is short since in that case
the overhead of creating a custom huffman code is more than is gained
by that custom code.

3. compressed with a custom Huffman code - The block contains
a description of a Huffman code to be used in this block only
and then a Huffman-code LZ77 compressed bitsequence.  The values
that describe the custome huffman tree are themselves huffman coded.
  


|#

(in-package :zip)

(defun inflate (p op)
  ;; user callable
  ;; inflate the stream p into the stream op
  ;; both streams should be unsigned-byte 8
  ;;
  (let ((br (new-bit-reader p))
	(buffer (make-array (* 32 1024) :element-type '(unsigned-byte 8)))
	(end 0))
    (loop
      (if* (null (setq end (process-deflate-block br op buffer end)))
	 then ; last block, we're all done
	      (return)))))




;;; ------------ gzip support
;
; gzip preceeds files with a header and the only support we need
; give to handle gzip files is the ability to skip the header
; and get to the meat of the file


; gzip constants

; compression strategies (only one supported)
(defconstant z_deflated 8)

; flag bits
(defconstant gz_ascii_flags #x01)   ; file probably ascii
(defconstant gz_head_crc    #x02)   ; header crc present
(defconstant gz_extra_field #x04)   ; extra field present
(defconstant gz_orig_name   #x08)   ; original file name present
(defconstant gz_comment     #x10)   ; file comment present
(defconstant gz_reserved    #xe0)   ; no bits allowed on here

(defun skip-gzip-header (p)
  ;; If the next thing in the stream p is gzip header then skip
  ;; past it and return t.
  ;; If it's not a gzip header than return nil
  ;; If it's starts to look like a gzip header but turns out to 
  ;; not be valid signal an error.  Note that the first byte of
  ;; a gzip header is an illegal byte to begin a deflated stream so
  ;; that if the first byte matches a gzip header but the rest do not
  ;; then the stream was positioned at neither a gzip header nor a
  ;; deflated stream
  ;
  ;; see check_header in gzio.c in rpm zlib-1.1.3 (or variant)
  ;; for details on what's in the header.
  
  (let (method flags)
    
    ; look for magic number
    (if* (not (eql #x1f (read-byte p)))
       then ; not a gzip header, may be a deflate block
	    (unread-char (code-char #x1f) p)
	    (return-from skip-gzip-header nil))
    

    ; now check the second magic number
    (if* (not (eql #x8b (read-byte p)))
       then (error "non gzip magic number"))
  
    (setq method (read-byte p)
	  flags  (read-byte p))

    (if* (or (not (eql method z_deflated))
	     (not (zerop (logand flags gz_reserved))))
       then (error "bad method/flags in header"))
  
    ; discard time, xflags and os code */
    (dotimes (i 6) (read-byte p))
  
    ; discard extra field if present
    (if* (logtest flags gz_extra_field)
       then (let ((length (+ (read-byte p)
			     (ash (read-byte p) 8))))
	      (dotimes (i length) (read-byte p))))
  
    (if* (logtest flags gz_orig_name)
       then ; discard name of file, null terminated
	    (do ((val (read-byte p) (read-byte p)))
		((zerop val))))
  
    (if* (logtest flags gz_comment)
       then ; discard comment, null terminated
	    (do ((val (read-byte p) (read-byte p)))
		((zerop val))))
  
    (if* (logtest flags gz_head_crc)
       then ; discard header crc
	    (dotimes (i 2) (read-byte p)))

    ; success!
    t	
    ))
		
;;;----------- end gzip support



;;;----------- support for reading bitfields from a stream
  
  
(defstruct bit-reader 
  stream
  last-byte	; last byte read, possibly two combined bytes too
  bits		; bits left of last byte to use
  )

(defparameter *maskarray*
    ;; for a bit length, mask off junk bits
    (make-array 17 
		 :initial-contents 
		 '(#x0 
		   #x1    #x3    #x7    #xf
		   #x1f   #x3f   #x7f   #xff
		   #x1ff  #x3ff  #x7ff  #xfff
		   #x1fff #x3fff #x7fff #xffff)))

;; bit reader
(defun new-bit-reader (stream)
  ; create and initialize bit reader
  (make-bit-reader :stream stream :last-byte 0 :bits 0))

(defun reset-bit-reader (br)
  ; clear out unused bit of the current byte
  (setf (bit-reader-bits br) 0))

(defun read-bits (br count)
  ;; return a value from the current bit reader.
  ;; the count can be from 1 to 16
  ;;
  
  (if* (eql count 0)
     then (return-from read-bits 0))
  
  
  (let ((last-byte (bit-reader-last-byte br))
	(bits      (bit-reader-bits br)))
    (loop 
      (if* (>= bits count)
	 then ;we have enough now
	      (if* (> bits count)
		 then ; we have some left over
		      (setf (bit-reader-last-byte br)
			(ash last-byte (- count)))
		      (setf (bit-reader-bits br) (- bits count))
		      (return (logand last-byte (svref *maskarray* count)))
		 else ; no bits left
		      (setf (bit-reader-bits br) 0)
		      (setf (bit-reader-last-byte br) 0)
		      (return last-byte)
		      )
	 else ; need a new byte
	      (let ((new-byte (read-byte (bit-reader-stream br))))
		(setq last-byte (+ last-byte
				   (ash new-byte bits)))
		(incf bits 8))))))



;;;----------- end bitfield reading




;;;----------- build constant tables needed by the algorithm

;; The tables needed to decode length and distance values
;; A compressed file contains a sequence of literal character values
;; or (length,distance) pairs.  The length is computed by taking
;; the length-value in the file and using these tables to bind
;; a base length value and the number of extra bits to read from the file
;; and then to add to the length value.
;; The same is done for distance.

(defvar *base-length*) ; array mapping code to length value
(defvar *length-extra-bits*) ; array saying how many more bitsworth to read

(defvar *base-distance*)
(defvar *distance-extra-bits*)


; build those arrays at load time:

(progn
   (setq *base-length* (make-array (1+ (- 285 257)))
	 *length-extra-bits* (make-array (1+ (- 285 257))))
  
   (let ((len 3)
	 (ind 0))
     (dolist (ent '((8 0)  ; count and number of extra bits
		    (4 1) (4 2) (4 3) (4 4) (4 5) (1 0)))
       (dotimes (i (car ent)) 
	 (setf (svref *base-length* ind) len)
	 (setf (svref *length-extra-bits* ind) (cadr ent))
	 (incf ind 1)
	 (incf len (ash 1 (cadr ent)))
	 )
       ; special case, code 285 is length 258.  
       (setf (svref *base-length* (- 285 257)) 258)
       ))

   (setq *base-distance* (make-array (1+ (- 29 0)))
	 *distance-extra-bits* (make-array (1+ (- 29 0))))
  
   (let ((dist 1)
	 (ind 0))
     (dolist (ent '((4 0) ; count and number of extra bits
		    (2 1) (2 2) (2 3) (2 4) (2 5) (2 6) (2 7) (2 8)
		    (2 9) (2 10) (2 11) (2 12) (2 13)))
       (dotimes (i (car ent))
	 (setf (svref *base-distance* ind) dist)
	 (setf (svref *distance-extra-bits* ind) (cadr ent))
	 (incf ind 1)
	 (incf dist (ash 1 (cadr ent)))))))




;;;----------- end table building



;;;----------- Huffman tree support

(defstruct (bitinfo (:type list))
  ;; when we describe a range of values and the code width we
  ;; use a list of three elements.  this structure describes it
  minval
  maxval
  bitwidth)


;test case
; (generate-huffman-tree '((0 4 3) (5 5 2) (6 7 4)))
; will generate sample table from the Deutsch paper
;

(defun generate-huffman-tree (bitinfo)
  ;; bitinfo is a list of bitinfo items (minval maxval bitwidth)
  ;; which means that values from minval through maxval are
  ;; to be represented by codes of width bitwidth.
  ;;
  ;; we return two valuse: the huffman tree and the mininum bit width
  ;;
  (let ((maxval 0)
	(minval most-positive-fixnum)
	(maxbitwidth 0)
	(minbitwidth most-positive-fixnum)
	bitwidthcounts
	valuecode
	valuewidth
	nextcode
	)
    ; find out the range of values (well the max) and the max bit width
    (dolist (bi bitinfo)
      (setq maxval (max maxval (bitinfo-maxval bi)))
      (setq minval (min minval (bitinfo-minval bi)))
      (setq maxbitwidth (max maxbitwidth (bitinfo-bitwidth bi)))
      (setq minbitwidth (min minbitwidth (bitinfo-bitwidth bi)))
      )
  
    ; per bitwidth arrays
    (setq bitwidthcounts (make-array (1+ maxbitwidth) 
				     :initial-element 0))
    (setq nextcode (make-array (1+ maxbitwidth) 
			       :initial-element 0))
  
    ; per value arrays
    (setq valuecode (make-array (1+ (- maxval minval)))) ; huffman code chose
    (setq valuewidth (make-array (1+ (- maxval minval))
				 :initial-element 0)) ; bit width
  
    (dolist (bi bitinfo)
      ; set valuewidth array from the given data
      (do ((v (bitinfo-minval bi) (1+ v)))
	  ((> v (bitinfo-maxval bi)))
	(setf (svref valuewidth (- v minval)) (bitinfo-bitwidth bi)))
    
      ; keep track of how many huffman codes will have a certain bit width
      (incf (svref bitwidthcounts (bitinfo-bitwidth bi))
	    (1+ (- (bitinfo-maxval bi) (bitinfo-minval bi))))
      )
  
  
  
    ; compute the starting code for each bit width
    (let ((code 0))
      (dotimes (widthm1 maxbitwidth)
	(setq code 
	  (ash (+ code (svref bitwidthcounts widthm1)) 1))
	(setf (svref nextcode (1+ widthm1)) code)))
  
    ; compute the huffman code for each value
    (do ((v minval (1+ v)))
	((> v maxval))
      (let ((width (svref valuewidth (- v minval))))
	(if* (not (zerop width))
	   then ; must assign a code
		(setf (svref valuecode (- v minval))
		  (svref nextcode width))
		(incf (svref nextcode width)))))

    ;; now we know the code for each value in the valuecode array
    ;;
    ;; now compute the tree
    (values (build-huffman-tree 
	     minval
	     (mapcar #'(lambda (bi) (cons (car bi) (cadr bi))) bitinfo)
	     valuecode valuewidth 1)
	    ; second value useful for decoding:
	    minbitwidth)))


(defun build-huffman-tree (minval minmaxes valuecode valuewidth pos)
  ;; compute a huffman cons tree
  ;; minmaxes is a list of conses. each cons 
  ;; representing a (min . max) range of values.
  ;;
  
  (multiple-value-bind (zero one) (split-on-position minval minmaxes 
						     valuecode
						     valuewidth
						     pos)
    (cons (if* (consp zero)
	     then (build-huffman-tree minval 
				      zero valuecode valuewidth (1+ pos))
	     else zero)
	  (if* (consp one)
	     then (build-huffman-tree minval one valuecode valuewidth (1+ pos))
	     else one))))

(defun split-on-position (minval minmaxes valuecode valuewidth pos)
  ;; compute those values that have a zero in the pos (1 based) position
  ;; of their code and those that have one in that position.
  ;; return two values, the zero set and the one set.
  ;; The position is from the msbit of the huffman code.
  ;;
  ;; If the value of the specified pos selects a specific value
  ;; and no further bits need be read to identify that value then
  ;; we return that value rather than a list of conses.
  
  (let (zero one)
    (dolist (mm minmaxes)
      (do ((v (car mm) (1+ v)))
	  ((> v (cdr mm)))
	(let ((width (svref valuewidth (- v minval)))
	      (code  (svref valuecode  (- v minval))))
	  (if* (logbitp (- width pos) code)
	     then ; one bit set
		  (if* (eql width pos)
		     then ; last bit
			  (setq one v)
		     else ; more bits to check
			  (let ((firstone (car one)))
			    (if* (and firstone 
				      (eq (cdr firstone) (1- v)))
			       then ; increase renge
				    (setf (cdr firstone) v)
			       else (push (cons v v) one))))
	     else ; zero bit set
		  (if* (eql width pos)
		     then ; last bit
			  (setq zero v)
		     else ; more bits to check
			  (let ((firstzero (car zero)))
			    (if* (and firstzero
				      (eq (cdr firstzero) (1- v)))
			       then ; increase renge
				    (setf (cdr firstzero) v)
			       else (push (cons v v) zero))))))))
    (values 
     (if* (consp zero) then (nreverse zero) else zero) ; order numerically
     (if* (consp one)  then (nreverse one)  else one))))


(defun generate-huffman-tree-from-vector (vector start end)
  ;; generate huffman tree from items in the vector from start to end-1
  ;; assume start corresponds to value 0 in the tree
  (do ((i start (1+ i))
       (val 0 (1+ val))
       (res))
      ((>= i end)
       (generate-huffman-tree (nreverse res)))
    (let ((len (svref vector i)))
      (if* (> len 0) 
	 then (push (list val val len) res)))))

      
  
  

;; the huffman tree to use for type 1 blocks
;;
(defparameter *fixed-huffman-tree* 
    (generate-huffman-tree '((0 143 8) (144 255 9) (256 279 7) (280 287 8))))

;; distance are represented by a trivial huffman code
(defparameter *fixed-huffman-distance-tree* 
    (generate-huffman-tree '((0 31 5))))


;;;----------- end Huffman support




(defun process-deflate-block (br op buffer end)
  ;; br is a bit stream, op is the output stream
  ;; process the next block in the stream
  ;; return false if this is the last block of data else
  ;; return the next index into the buffer
  (let ((bfinal (read-bits br 1))
	(btype  (read-bits br 2)))
    
    (setq end
      (case btype
	(0 (process-non-compressed-block br op buffer end))
	(1 (process-fixed-huffman-block br op buffer end))
	(2 (process-dynamic-huffman-block br op buffer end))
	(3 (error "illegal deflate block value"))))
    (if* (eql bfinal 1) 
       then (flush-buffer op buffer end)
	    nil
       else end)
    ))



(defun process-non-compressed-block (br op buffer end)
  ;; process a block of uncompressed data
  (reset-bit-reader br)
  (let ((p (bit-reader-stream br)))
    (let ((len (read-uword p))
	  (onecomplen (read-uword p)))
      (if* (not (eql len (logxor #xffff onecomplen)))
	 then (error "bad length value in non compressed block"))
      (dotimes (i len)
	(setq end (put-byte-in-buffer op (read-byte p) buffer end))))
    end))

(defun read-uword (stream)
  ;; read a little endian value
  (+ (read-byte stream) (ash (read-byte stream) 8)))

(defun put-byte-in-buffer (op byte buffer end)
  ;; store the next output byte in the buffer
  (if* (>= end (length buffer))
     then (flush-buffer op buffer end)
	  (setq end 0))
  (setf (aref buffer end) byte)
  (1+ end))

(defun flush-buffer (op buffer end)
  ;; send bytes to the output stream. If op isn't a stream
  ;; then it must be a function to funcall to take the bytes.
  (if* (> end 0) 
     then (if* (streamp op)
	     then (write-sequence buffer op :end end)
	     else (funcall op buffer end))))


  


(defun process-fixed-huffman-block (br op buffer end)
  ;; process a huffman block with the standard huffman tree
  ;;
  (process-huffman-block br op *fixed-huffman-tree* 7 *fixed-huffman-distance-tree* 5
			 buffer end))

(defun process-huffman-block (br op 
			      lengthlit-tree minwidth 
			      distance-tree mindistwidth
			      buffer end)
  ;; the common code for blocks of type 1 and 2 that does
  ;; the decompression given  a length/literal huffman tree
  ;; and a distance huffman tree.
  ;; If the distance tree is nil then we use the trivial huffman 
  ;; code from the algorithm.
  ;;
  (let* ((bufflen (length buffer))
	 length
	 distance
	 )
    
		 
    (loop
      (let ((value (decode-huffman-tree br lengthlit-tree minwidth)))
	(if* (< value 256)
	   then ; output and add to buffer
		(setq end (put-byte-in-buffer op value buffer end))
		
	 elseif (eql value 256) 
	   then (return) ; end of block
	   else ; we have a length byte
		; compute length, distance
		  
		(let ((adj-code (- value 257)))
		  (setq length (+ (svref *base-length* adj-code)
				  (read-bits br (svref *length-extra-bits*
						       adj-code)))))
		
		(let ((dist-code (if* distance-tree
				    then (decode-huffman-tree br
							      distance-tree
							      mindistwidth)
				    else (read-bits br 5))))
		  (setq distance 
		    (+ (svref *base-distance* dist-code)
		       (read-bits br (svref *distance-extra-bits*
					    dist-code)))))
		  
		; copy in bytes
		(do ((i (mod (- end distance) bufflen) (1+ i))
		     (count length (1- count)))
		    ((<= count 0))
		  (if* (>= i bufflen) then (setf i 0))
		  (setq end (put-byte-in-buffer op
						(aref buffer i)
						buffer
						end))))))
    ; return where we left off
    end))
		    
		

(defparameter *code-index*
    ;; order of elements in the code index values
    ;; pretty crazy, eh?
    (make-array 19 
		:initial-contents
		'(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)))

		  
(defun process-dynamic-huffman-block (br op buffer end)
  ;; process a block that includes a personalized huffman tree
  ;; just for this block
  (let ((hlit (read-bits br 5))
	(hdist (read-bits br 5))
	(hclen (read-bits br 4))
	
	code-length-huffman-tree
	(minlen 9999) 
	)
    
    ; read in the huffman code width of each of the numbers
    ; from 0 18... this will be then used to create a huffman tree
    ;
    (let ((codevec (make-array 19 :initial-element 0))
	  (len))
      
      (dotimes (i (+ hclen 4))
	(setf (svref codevec 
		     (svref *code-index* i))
	  (setq len (read-bits br 3)))
	(if* (> len 0) then (setq minlen (min len minlen))))
      
      
      
      (setq code-length-huffman-tree 
	(generate-huffman-tree-from-vector codevec 0 (length codevec))))
    
    ; now we're in position to read the code lengths for the
    ; huffman table that will allow us to read the data.
    ; (Is this a nutty algorithm or what??)
    ;
    (let ((bigvec (make-array (+ hlit 257 hdist 1)
			      :initial-element 0))
	  (index 0))
      (loop
	(if* (>= index (length bigvec)) then (return))
	(let ((val (decode-huffman-tree br code-length-huffman-tree minlen)))
	  (if* (<= val 15)
	     then ; literal value
		  (setf (svref bigvec index) val)
		  (incf index)
	   elseif (eql val 16)
	     then ; repeat prev
		  (let ((prev-val (svref bigvec (1- index))))
		    (dotimes (i (+ 3 (read-bits br 2)))
		      (setf (svref bigvec index) prev-val)
		      (incf index)))
	   elseif (eq val 17)
	     then ; repeat zero
		  (dotimes (i (+ 3 (read-bits br 3)))
		    (setf (svref bigvec index) 0)
		    (incf index))
	   elseif (eq val 18)
	     then ; repeat zero a lot 
		  (dotimes (i (+ 11 (read-bits br 7)))
		    (setf (svref bigvec index) 0)
		    (incf index)))))
      
      (let (literal-length-huffman litlen-width
	    distance-huffman distance-width)
	(multiple-value-setq (literal-length-huffman litlen-width)
	  (generate-huffman-tree-from-vector bigvec 0 (+ hlit 257)))
      
	(multiple-value-setq (distance-huffman distance-width)
	  (generate-huffman-tree-from-vector bigvec (+ hlit 257) 
					     (length bigvec)))
      
	(process-huffman-block br op literal-length-huffman litlen-width
			       distance-huffman distance-width
			       buffer end)
	))))



(defun decode-huffman-tree (br tree minbits)
  ;; find the next huffman encoded value.
  ; the minimum length of a huffman code is minbits so 
  ; grab that many bits right away to speed processing and the
  ; go bit by bit until the answer is found
  (let ((startval (read-bits br minbits)))
    (dotimes (i minbits)
      (if* (logtest 1 startval)
	 then (setq tree (cdr tree))
	 else (setq tree (car tree)))
      (setq startval (ash startval -1)))
    (loop
      (if* (atom tree)
	 then (return tree)
	 else (if* (eql 1 (read-bits br 1))
		 then (setq tree (cdr tree))
		 else (setq tree (car tree)))))))





    
;;; test case...
;; Read file created with gzip and write the uncompressed version
;; to another file.  
;;
;; Porting note: the open below works on ACL since it creates
;;   a bivalent simple-stream.   If you run this on other lispsj
;;   you'll want to specify an :element-type of '(unsigned-byte 8)
;;
#+ignore
(defun testit (&optional (filename "foo.n.gz") (output-filename "out"))
  (with-open-file (p filename :direction :input)
    (skip-gzip-header p)
    (with-open-file (op output-filename :direction :output
		     :if-exists :supersede)
      (inflate p op))))
