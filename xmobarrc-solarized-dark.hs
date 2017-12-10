Config {-- font = "xft:Gilius ADF No2:size=11"
        font = "xft:Meslo LG L DZ:size=11"
        , bgColor = "#073642"
        , fgColor = "#657b83"
        , position = Static { xpos=0,ypos=0,width=2560,height=36}
        , commands = [ Run Weather "EGLC" ["-t","<tempC>°C","-L","10","-H","21","--normal","#b58900","--high","#b58900","--low","#268bd2"] 36000
                        , Run Cpu ["-t","<total>⚙","-L","30","-H","70","--low","#93a1a1","--normal","#b58900","--high","#cb4b16"] 10
                        , Run Memory ["-t","<usedratio>Ξ"] 10
                        , Run Battery ["-t","<left><acstatus> <timeleft>",
                                       "-L","30",
                                       "-H","80",
                                       "--low","#dc322f,#002b36",
                                       "--normal","#b58900,#002b36",
                                       "--high","#93a1a1,#002b36",
                                       "--",
                                       "-O","↑",
                                       "-o","↓",
                                       "-i","~"
                                       ] 100
                        , Run Date "%a %b %_d %Y %H:%M" "date" 100
                        , Run StdinReader
                        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%StdinReader% }{ <action=`urxvt -e top`> %cpu% %memory% </action><fc=#586e75,#002b36> %battery% </fc> <fc=#586e75>%date%</fc> %EGLC%       "

        , persistent   = True
        , lowerOnStart = True
        , hideOnStart  = False
        }
