sub test_sub ··· endsub

call test_sub

sprite new 0, "player"
sprite pos 0, (100, 100)
sprite image 0, "skeleton"
sprite move 0, (1, 1)
sprite del 0
sprite var 0, 0
sprite center 0, (8,8)
sprite size 0, (8, 8)

screen load "logo"

if [not] spritecol(0,0) then ··· [else ···] endif
if [not] spritevar(0,0)=1 then ··· [else ···] endif
if [not] spritevar(0,0)<1 then ··· [else ···] endif
if [not] spritevar(0,0)<=1 then ··· [else ···] endif
if [not] spritevar(0,0)>=1 then ··· [else ···] endif
if [not] spritex(0)=2 then ··· [else ···] endif
if [not] spritex(0)<2 then ··· [else ···] endif
if [not] spritex(0)>2 then ··· [else ···] endif
if [not] spritex(0)<=2 then ··· [else ···] endif
if [not] spritex(0)>=2 then ··· [else ···] endif
if [not] spritey(0)=3 then ··· [else ···] endif
if [not] spritey(0)<3 then ··· [else ···] endif
if [not] spritey(0)>3 then ··· [else ···] endif
if [not] spritey(0)<=3 then ··· [else ···] endif
if [not] spritey(0)>=3 then ··· [else ···] endif

tilemap load "map1"
tilemap load "map1", (0,0)
tilemap scroll right
tilemap scroll left
tilemap scroll up
tilemap scroll down
tilemap pos X,Y

if spritetile(0,16,8)=1 then ··· endif

if [not] keydown("left") then ··· [else ···] endif
if [not] keydown("right") then ··· [else ···] endif
if [not] keydown("up") then ··· [else ···] endif
if [not] keydown("down") then ··· [else ···] endif
if [not] keydown("fire1") then ··· [else ···] endif
if [not] keydown("fire2") then ··· [else ···] endif
if [not] keypressed("left") then ··· [else ···] endif
if [not] keypressed("right") then ··· [else ···] endif
if [not] keypressed("up") then ··· [else ···] endif
if [not] keypressed("down") then ··· [else ···] endif
if [not] keypressed("fire1") then ··· [else ···] endif
if [not] keypressed("fire2") then ··· [else ···] endif

sfx play 0
sfx stop
sft unmute

music play "gamemusic"
music stop
