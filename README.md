<!DOCTYPE html>
<html>
<body>
<h1>2D Lid Driven Cavity Incompressible Solver</h1>

<h1>UNIFORM GRID</h1>

 <pre>
 Input data:
 Grid information
     grid_file = grid.ini: 81x81  
 
 Flow parameters
     Re_ref = 200         
 
 Computational parameters
     dt = 0.0025       
     niter = 4000        
     niter_gauss = 20          
     tolerance = 1.0e-6      
     relax_u = 0.5         
     relax_v = 0.5         
     relax_p = 0.2  
</pre>

<p>Grid, u-velocity contour, and v-velocity contour:</p>
<div class="image-container">
    <img src="https://github.com/user-attachments/assets/3a9465e3-da26-4601-8c6f-fcf5a55ffd2c" width="250" alt="Example Image">
    <img src="https://github.com/user-attachments/assets/fe04c502-a444-4ad2-aa31-05708f157584" width="300">
    <img src="https://github.com/user-attachments/assets/336c4aee-b8ec-4d01-bbf9-f484525aa636" width="300">
</div>


<h1>NON-UNIFORM GRID</h1>

<pre>
 Input data:
 Grid information
     grid_file = grid.ini: 80x80  
 
 Flow parameters
     Re_ref = 200         
 
 Computational parameters
     dt = 0.00001     
     niter = 1000000     
     niter_gauss = 20          
     tolerance = 1.0e-6      
     relax_u = 0.5         
     relax_v = 0.5         
     relax_p = 0.2 
</pre>

<p>Grid, u-velocity contour, and v-velocity contour:</p>
<div class="image-container">
    <img src="https://github.com/user-attachments/assets/f6951ed0-61a8-4a3f-a650-13da77cbca8a" width="250">
    <img src="https://github.com/user-attachments/assets/f16717cb-2194-427a-b1fe-ef2761622ac8" width="300">
    <img src="https://github.com/user-attachments/assets/a96179e3-ab33-4920-af22-749626a1dcc7" width="300">
</div>
