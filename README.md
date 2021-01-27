# Sample-CFA-and-EFA-RCode
I wrote this sample code for a client to help them evaluate the validity of their survey measurements. 

My client have three different scales measuring the influence of tinnitus on patients.
The first one is the THI scale which have 25 questions in total. Among these 25 questions, 12 measure the functional influence, 8 measure the emotional influence, and 5 measure the catastrophic influences. These (Functional, Emotional, Catastrpphic) are called latent variables in CFA terms but I just call them 'subscales' in the comments of my R code.

The second one is the TFI scale that also have 25 questions in total, but 8 subscales (I: intrusive, Sc:sense of control, C: cognitive, Sl: sleep, A: auditory, R: relaxation, Q: quality of life, E: emotional)

The third one is the TPFQ scale with four subsales (Concentration, Emotion, Hearing, Sleep) and 20 questions in total.

Before running CFA and EFA, I actually evaluate the framing of each questions in terms of face validity, which help to guide the direction of factor analysis.