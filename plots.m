%{
% Not needed now since the output from the solver 
% is without the ghost nodes.
u=u(2:end-1,2:end-1);
v=v(2:end-1,2:end-1);
p=p(2:end-1,2:end-1);
%}

% For uniform grid execute following
%dx = x_grid(1,2)-x_grid(1,1);
%dy = y_grid(2,1)-y_grid(1,1);

% For non-uniform grid: copy the dx and dy matrices 
% from log file and execute following.
%dx = repmat(dx(2:end),size(x_grid,1),1);
%dy = repmat(dy(2:end)',1, size(y_grid,2));


%% u-velocity
% u-velocity contour
x = x_grid+dx/2;
x = x(:,1:end-1);
y = y_grid(:,1:end-1);
u_interp = interp2(x,y,u,x_grid,y_grid);
% Boundary Conditions
u_interp(1:end-1,1) = 0; u_interp(1:end-1,end) = 0;
u_interp(end,1) = u_interp(end,2); u_interp(end,end) = u_interp(end,end-1);

figure('name','1', 'color', [1,1,1], ...
       'position',[200,200,300,250]);
contourf(x_grid,y_grid,u_interp,20,'EdgeAlpha',0)
set(gca, 'fontsize',10, 'fontweight','bold');
xlabel('x/L_{ref}'); ylabel('y/L_{ref}');
colorbar; colormap("jet")
axis('equal')

% u-velocity vs y at x = 0.5
figure('name','2', 'color', [1,1,1], ...
       'position',[200,200,250,240]);
ind = ceil(size(x_grid,2)/2); 
plot(u_interp(:,ind),y_grid(:,ind),'-k','linewidth',2)
set(gca, 'fontsize',10, 'fontweight','bold');
xlabel('u/u_{\infty}'); ylabel('y/L_{ref}');
grid minor


%% v-velocity
% v-velocity contour
y = y_grid+dy/2;
y = y(1:end-1,:);
x = x_grid(1:end-1,:);
v_interp = interp2(x,y,v,x_grid,y_grid);
% Boundary Conditions
v_interp(1,:) = 0; v_interp(end,:) = 0;

figure('name','3', 'color', [1,1,1], ...
       'position',[200,200,300,250]);
contourf(x_grid,y_grid,v_interp,20,'EdgeAlpha',0)
set(gca, 'fontsize',10, 'fontweight','bold');
xlabel('x/L_{ref}'); ylabel('y/L_{ref}');
colorbar; colormap("jet")
axis('equal')

% v-velocuty vs x at y=0.5
figure('name','4', 'color', [1,1,1], ...
       'position',[200,200,250,240]);
ind = ceil(size(y_grid,1)/2); 
plot(x_grid(ind,:),v_interp(ind,:),'-k','linewidth',2)
set(gca, 'fontsize',10, 'fontweight','bold');
xlabel('x/L_{ref}'); ylabel('v/u_{\infty}'); 
grid minor


%% Pressure
% Pressure contour
figure('name','5', 'color', [1,1,1], ...
       'position',[200,200,300,250]);
p_mean = mean(mean(p));
contourf(x_grid,y_grid, p-p_mean,-2:0.01:2,EdgeAlpha=0.3)
set(gca, 'fontsize',10, 'fontweight','bold');
xlabel('x/L_{ref}'); ylabel('y/L_{ref}');
colorbar; colormap("jet")
axis('equal')


%% Convergence history
% RMS difference 
figure('name','6', 'color', [1,1,1], ...
       'position',[200,200,250,235]); hold on;
plot(history(:,1),history(:,4),'-k','linewidth',2,'DisplayName','u')
plot(history(:,1),history(:,5),'-r','linewidth',2,'DisplayName','v')
plot(history(:,1),history(:,6),'-g','linewidth',2,'DisplayName','p')
set(gca, 'fontsize',10, 'fontweight','bold');
xlabel('x/L_{ref}'); ylabel('y/L_{ref}');
set(gca, 'YScale', 'log');
legend; box; grid minor;


%% Velocity vector field
indr = [1:2:size(x_grid,1)-1,size(x_grid,1)];
indc = [1:2:size(x_grid,2)-1,size(x_grid,2)];
figure('name','7', 'color', [1,1,1], ...
       'position',[200,200,250,235]); hold on;
quiver(x_grid(indr,indc),   y_grid(indr,indc), ...
       u_interp(indr,indc), v_interp(indr,indc), 2 ...
       , ...
       'Color','k','LineWidth',1);
set(gca, 'fontsize',10, 'fontweight','bold');
xlabel('x/L_{ref}'); ylabel('y/L_{ref}');
box; xlim([0,1]); ylim([0,1]); 


%% Grid
figure('name','7', 'color', [1,1,1], ...
       'position',[200,200,250,235]); hold on;
surf(x_grid,y_grid,u_interp,'FaceColor','none');
set(gca, 'fontsize',10, 'fontweight','bold');
xlabel('x/L_{ref}'); ylabel('y/L_{ref}');
box; xlim([0,1]); ylim([0,1]); axis equal;