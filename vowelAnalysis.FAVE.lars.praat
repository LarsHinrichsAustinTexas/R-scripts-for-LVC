
##########################################################
# Analyze Vowels from a .wav file via a forced-aligned TextGrid          
# created using FAVE (fave.ling.upenn.edu)                                 
# This script April 2012 by Lars Hinrichs, Doug Bigham, Kate Points
# based on scripts by Robert Felty and Mietta Lennes
# contact: lh@utexas.edu, www.lars-hinrichs.net  
##########################################################



# Read from file... /Users/....wav
# Read from file... /Users/....TextGrid




# R E A D    S E L E C T E D    S O U N D    A N D    T E X T G R I D
# (TextGrid must be forced-aligned output from FAVE)
use_sound$ = selected$ ("Sound")
use_TextGrid$ = selected$ ("TextGrid")
select TextGrid 'use_TextGrid$'
clearinfo



# VALUES TO SET MANUALLY
printline speaker	vowel	word	nucleusF1	nucleusF2	nucleusF3	midF1	midF2	midF3	glideF1	glideF2	glideF3	preSegment	folSegment	file	begin	duration

speaker$ = "Pastor Paul"
#gender$ = "male"
#region$ = "ET"


tierForPhone = 1
tierForWord = 2
#tierForStyle = 

select Sound 'use_sound$'
To Formant (burg)... 0 5 5000 0.025 50





# V O W E L    A N A L Y S I S
select TextGrid 'use_TextGrid$'
nintervals = Get number of intervals... tierForPhone
for i to nintervals
	select TextGrid 'use_TextGrid$'
	label$ = Get label of interval... tierForPhone 'i'
	# next line selects all non-empty intervals, but you can put a vowel label between the...
	#if length(label$) > 2
		if endsWith(label$, "1")

			j = i - 1
			k = i + 1
			preseg$ = Get label of interval... tierForPhone 'j'
			folseg$ = Get label of interval... tierForPhone 'k'

			# S E T     T I M E S
			start = Get starting point... tierForPhone 'i'
			end = Get end point... tierForPhone 'i'
			dur = (end - start)
			duration = dur * 1000
			midpoint = start + ((end - start)/2)
			nucleus = start + (dur/5)
			glide = end - (dur/5)
    			intervalForWord = Get interval at time... tierForWord midpoint
    			word$ = Get label of interval... tierForWord intervalForWord
			# intervalForStyle = Get interval at time... tierForStyle midpoint
			# style$ = Get label of interval... tierForStyle intervalForStyle


			# M E A S U R E M E N T S
			select Formant 'use_sound$'
			nucleusf1 = Get value at time... 1 nucleus Hertz Linear
			nucleusf2 = Get value at time... 2 nucleus Hertz Linear
			nucleusf3 = Get value at time... 3 nucleus Hertz Linear
			midf1 = Get value at time... 1 midpoint Hertz Linear
			midf2 = Get value at time... 2 midpoint Hertz Linear
			midf3 = Get value at time... 3 midpoint Hertz Linear
			glidef1 = Get value at time... 1 glide Hertz Linear
			glidef2 = Get value at time... 2 glide Hertz Linear
			glidef3 = Get value at time... 3 glide Hertz Linear




			# O U T P U T
			printline 'speaker$'	'label$'	'word$'	'nucleusf1:1'	'nucleusf2:1'	'nucleusf3:1'	'midf1:1'	'midf2:1'	'midf3:1'	'glidef1:1'	'glidef2:1'	'glidef3:1'	'preseg$'	'folseg$'	'use_sound$'	'start:1'	'duration:0'	

		endif
	#endif
endfor


select Formant 'use_sound$'
Remove
#select Sound 'use_sound$'
#Remove
#select TextGrid 'use_TextGrid$'
#Remove


