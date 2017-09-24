Config {-- font = "xft:Gilius ADF No2:size=11"
        font = "xft:Meslo LG L DZ:size=11"
        , bgColor = "#eee8d5"
        , fgColor = "#839496"
        , position = Static { xpos=0,ypos=0,width=2560,height=36}
        , commands = [ Run Weather "EGLC" ["-t","<tempC>°C","-L","10","-H","21","--normal","#b58900","--high","#b58900","--low","#268bd2"] 36000
                        , Run Cpu ["-t","<total>⚙","-L","30","-H","70","--low","#93a1a1","--normal","#b58900","--high","#cb4b16"] 10
                        , Run Memory ["-t","<usedratio>Ξ"] 10
                        , Run Battery ["-t","<left><acstatus> <timeleft>",
                                       "-L","30",
                                       "-H","80",
                                       "--low","#dc322f,#fdf6e3",
                                       "--normal","#b58900,#fdf6e3",
                                       "--high","#93a1a1,#fdf6e3",
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
        , template = "%StdinReader% }{ <action=`urxvt -e top`> %cpu% %memory% </action><fc=#93a1a1,#fdf6e3> %battery% </fc> <fc=#93a1a1>%date%</fc> %EGLC%       "

        , persistent   = True
        , lowerOnStart = True
        , hideOnStart  = False
        }
