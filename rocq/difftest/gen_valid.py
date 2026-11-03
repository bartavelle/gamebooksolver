"""Writes random valid characters at each chapter of each book, one case per line: S for looking at one step, W for
following a random game. Some chapters get more, as a sample of all the chapters can miss them."""
import json,random,subprocess,sys,re
root,runner,n,w,seed,out=sys.argv[1],sys.argv[2],int(sys.argv[3]),int(sys.argv[4]),int(sys.argv[5]),sys.argv[6]
random.seed(seed)
W=["Dagger","Spear","Mace","ShortSword","Warhammer","Sword","Axe","Quarterstaff","BroadSword","MagicSpear","Sommerswerd"]
D=["Camouflage","Hunting","SixthSense","Tracking","Healing","MindShield","MindBlast","AnimalKinship","MindOverMatter"]
SP=["Shield","BodyArmor","Helmet"]
BP=["Potion2Hp","Potion4Hp","Potion5Hp","Potion6Hp","StrengthPotion","StrengthPotion4"]
ids={int(l.split()[0]):[int(x) for x in l.split()[1:]] for l in subprocess.check_output([runner,'chapters'],text=True).splitlines() if l.strip()}
# the chapters that gain items after a test, with what it takes to get there
TARGETS={2:[(40,[])],3:[(10,['Knowledge01']),(422,[])]}
L=[]
for b in range(1,6):
    t=open(f'{root}/json-chapters/book0{b}.json').read()
    gs=sorted(set(re.findall(r'"SGenCounter (\d+)"',t)));gb=sorted(set(re.findall(r'"BGenCounter (\d+)"',t)))
    fl=sorted(set(re.findall(r'"(Knowledge\d+|Special\d+|Poisonned2|HadCombat|StrengthPotionActive|PotentStrengthPotionActive|LimbDeath|FoughtElix|ReceivedCrystalStarPendant)"',t))|{"HadCombat","Poisonned2"})
    def slots(i): return 2 if b==4 and i in('BGen3','BGen4') else 1
    def equip(pb=.85,bias=False):
        it={}
        if random.random()<pb: it['Backpack']=1
        for w_ in random.sample(W,random.choice([0,1,1,2,2])): it[w_]=1
        for s in SP:
            if random.random()<.25: it[s]=1
        for g in gs:
            if random.random()<.25 and not (g=='10' and 'Helmet' in it): it['SGen'+g]=1
        if random.random()<.8: it['Gold']=random.choice([random.randint(1,50),50,49])
        if 'Backpack' in it:
            room=8; full=random.random()<.4
            for p in BP+['BGen'+g for g in gb]:
                if room>=slots(p) and random.random()<(.5 if full else .2): it[p]=1;room-=slots(p)
            q=random.randint(0,min(room,3))
            if full: q=room if random.random()<.7 else q
            if q: it['Meal']=q;room-=q
            q=random.randint(0,min(room,2))
            if q and random.random()<.5: it['Laumspur']=q
        return it
    def char(c):
        # book + 4 disciplines, one of them being Weapon Skill most of the time
        ws=random.random()<.85
        ds=random.sample(D,4+b-(1 if ws else 0))
        if ws: ds.append('WS:'+random.choice(W[:9]))
        cs=random.randint(10,19);maxe=random.randint(20,29)
        it=equip()
        if b<=2 and c>=2000: it.pop('Sommerswerd',None)
        mh=maxe+(4 if 'BodyArmor' in it else 0)+(2 if 'Helmet' in it else 0)
        cur=random.randint(1,mh) if random.random()<.8 else mh
        f=[x for x in fl if random.random()<.12]
        pv=equip(.6) if random.random()<.2 else {}
        fm=lambda d:','.join(f'{k}:{v}' for k,v in d.items())
        return f"{b}|{cs}|{maxe}|{cur}|{c}|{','.join(ds)}|{fm(it)}|{','.join(f)}|{fm(pv)}"
    for c in ids[b]:
        for _ in range(n if c<2000 else 1):
            L.append("S|"+char(c))
    for c,fs in TARGETS.get(b,[]):
        for _ in range(n*30):
            p=char(c).split('|')
            p[7]=','.join(filter(None,[p[7]]+[x for x in fs if x not in p[7]]))
            L.append("S|"+'|'.join(p))
    for _ in range(w):
        L.append("W|"+char(random.choice([x for x in ids[b] if x<2000])))
open(out,'w').write('\n'.join(L)+'\n');print(len(L),'cases')
