"""Writes random characters at each chapter of each book, one case per line."""
import json,random,subprocess,sys
root=sys.argv[1];runner=sys.argv[2];n_real=int(sys.argv[3]);seed=int(sys.argv[4])
random.seed(seed)
W=["Dagger","Spear","Mace","ShortSword","Warhammer","Sword","Axe","Quarterstaff","BroadSword","MagicSpear","Sommerswerd"]
D=["Camouflage","Hunting","SixthSense","Tracking","Healing","MindShield","MindBlast","AnimalKinship","MindOverMatter"]
F=["PermanentSkillReduction","StrengthPotionActive","FoughtElix","LimbDeath","ReceivedCrystalStarPendant","Knowledge01","Knowledge02","Knowledge03","Knowledge04","Special01","Special02","Special03","Special04","Poisonned2","HadCombat","PermanentSkillReduction2","PotentStrengthPotionActive"]
SP=["Shield","BodyArmor","Helmet","SGen10"]
BP=["Potion2Hp","Potion4Hp","Potion5Hp","Potion6Hp","StrengthPotion","StrengthPotion4"]
ids={int(l.split()[0]):[int(x) for x in l.split()[1:]] for l in subprocess.check_output([runner,'chapters'],text=True).splitlines()}
import re
out=[]
for b in range(1,6):
    t=open(f'{root}/json-chapters/book0{b}.json').read()
    gs=sorted(set(re.findall(r'"SGenCounter (\d+)"',t)));gb=sorted(set(re.findall(r'"BGenCounter (\d+)"',t)))
    fl=[f for f in F if f in t or f in("HadCombat","Poisonned2","StrengthPotionActive","PotentStrengthPotionActive","LimbDeath")]
    def equip(backpack_p=.85):
        it={}
        if random.random()<backpack_p: it['Backpack']=1
        for w in random.sample(W,random.choice([0,1,1,2,2])): it[w]=1
        for s in SP:
            if random.random()<.2: it[s]=1
        for g in gs:
            if random.random()<.25: it['SGen'+g]=1
        if random.random()<.8: it['Gold']=random.randint(1,50)
        if 'Backpack' in it:
            room=8
            for p in BP+['BGen'+g for g in gb]:
                if room and random.random()<.2: it[p]=1;room-=1
            q=random.randint(0,min(room,3))
            if q: it['Meal']=q;room-=q
            q=random.randint(0,min(room,2))
            if q and random.random()<.5: it['Laumspur']=q
        return it
    for c in ids[b]:
        real=c<2000
        count=n_real if real else (1 if random.random()<.15 else 0)
        for _ in range(count):
            ds=random.sample(D,random.randint(4,8)); 
            if random.random()<.7: ds.append('WS:'+random.choice(W[:9]))
            cs=random.randint(8,28);maxe=random.randint(12,32)
            it=equip()
            # chapter 332 can't be reached with the Sommerswerd, see no_overflow.v
            if b<=2 and c>=2000: it.pop('Sommerswerd',None)
            mh=maxe+(4 if 'BodyArmor' in it else 0)+(2 if 'Helmet' in it else 0)
            cur=random.randint(1,mh) if random.random()<.8 else mh
            f=[x for x in fl if random.random()<.12]
            pv=equip(.6) if random.random()<.12 else {}
            fm=lambda d:','.join(f'{k}:{v}' for k,v in d.items())
            out.append(f"{b}|{cs}|{maxe}|{cur}|{c}|{','.join(ds)}|{fm(it)}|{','.join(f)}|{fm(pv)}")
open(sys.argv[5],'w').write('\n'.join(out)+'\n');print(len(out),'cases')
