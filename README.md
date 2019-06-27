# Out of the Tar Pit

Ben Moseley kindly shared the original source code. 

Downloaded from https://groups.google.com/forum/?fromgroups#!topic/frp-discuss/BNmBgtqRUFY

Read the paper https://github.com/papers-we-love/papers-we-love/blob/master/design/out-of-the-tar-pit.pdf

## Developing

Run container
`$ docker run -it -p 18888:18888 -v $PWD:/ootp ootp:latest`

Connect using telnet
`$ telnet localhost 18888`

Send test commands
`(1 (lookup-relation "" "parts"))`

## Watch me trying to launch it 🇷🇺

Explaining the paper, part 1: https://youtu.be/l-y63RNzxZQ  
Explaining the paper, part 2: https://youtu.be/hGr2OstSbGk  
Live streams fixing the source code: 

1. https://youtu.be/JL21QwIG5O4
2. https://youtu.be/QSLPZSoXsCo
