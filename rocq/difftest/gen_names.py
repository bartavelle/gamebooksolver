"""Generates names.ml, the names of the constructors of the extracted model."""
import re
m=open('model.mli').read()
def ctors(t):
    b=re.search(r'type '+t+r' =\n((?:\| .*\n)+)',m).group(1)
    return [re.match(r'\| (\w+)',l).group(1) for l in b.splitlines()]
w=ctors('weapon');s=ctors('sslot');i=[c for c in ctors('item') if c not in('Weapon','GenSpecial','GenBackpack')];d=[c for c in ctors('discipline') if c!='WeaponSkill'];f=ctors('flag')
o=['open Model','let item_of = function']
for c in w:o.append(f'| "{c}" -> Weapon {c}')
for c in s:
    n=c[1:]
    o.append(f'| "SGen{n}" -> GenSpecial {c}');o.append(f'| "BGen{n}" -> GenBackpack {c}')
for c in i:o.append(f'| "{c}" -> {c}')
o.append('| x -> failwith ("item "^x)')
o.append('let name_of = function')
for c in w:o.append(f'| Weapon {c} -> "{c}"')
for c in s:
    n=c[1:]
    o.append(f'| GenSpecial {c} -> "SGen{n}"');o.append(f'| GenBackpack {c} -> "BGen{n}"')
for c in i:o.append(f'| {c} -> "{c}"')
o.append('let disc_of = function')
for c in d:o.append(f'| "{c}" -> {c}')
for c in w:o.append(f'| "WS:{c}" -> WeaponSkill {c}')
o.append('| x -> failwith ("disc "^x)')
o.append('let flag_of = function')
for c in f:o.append(f'| "{c}" -> {c}')
o.append('| x -> failwith ("flag "^x)')
o.append('let flag_name = function')
for c in f:o.append(f'| {c} -> "{c}"')
open('names.ml','w').write('\n'.join(o)+'\n')
