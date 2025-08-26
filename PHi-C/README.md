
# Experiment with two Python programs, PHi-C and PHi-C2

This section is my record for experimenting with  
previous version of PHi-C and
newer version which is PHi-C2.  
(August 2025)

---

# PHi-C

## Environment

Mac OS mini M4  

JetBrains IDE PyCharm

## Source Code
GitHub [PHi-C Public Repository](https://github.com/soyashinkai)

## Screenshot

Running locally after all tutorial scripts 1 to 6 were run.  
Sample data is showing cordinates with each frequency.

![one](./one.png)

To show visualized result,  
Visual Molecular Dynamics (VMD) is required.

Registration is required but it is available for free.  
[Official Site](https://www.ks.uiuc.edu/Research/vmd/)

This shows an icon for latest Alpha 2.0 version under Mac OS x.

![two](./two.png)

In VMD application, load 2 files.  
- First [.psf] the stracutre file,  
- then [.xyz] the animation file.

![three](./three.png)

Render result will be shown.  
Sample is 'mouse embryonic stem cell data'  
'chromosome 8'

It can be move around and latate in 3 dimentions  
by using a mouse movement.

Also it will show animated result in this same window.

![four](./four.png)

This is VMD Main window.  
On the bottom, there are slider and  
animation controls are shown.

![five](./five.png)

---

# PHi-C2

Official code is avaialble here
[GitHub](https://github.com/soyashinkai/PHi-C2)

- To run a code under Apple M4 environment
- Use Miniconda
- Follow README.md instruction

# Result

After running each commend,<br>
under demo directory, new directory named<br>
ES_mapq30_KR_chr8_42100000-44525000_res25000bp<br>
will be created<br>

Under data_dynamic folder, there are 2 files as result
- polymer_N97.psf
- sample0.xyz

Load those files in VMD (Visual Molecular Dynamics) Software<br>
to get result as well as animation to run.<br>

## Result after running PHi-C2 and VMD

![six](./six.png)
