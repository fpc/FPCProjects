import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { AuthGuardService } from './auth-guard.service';
import { AdminGuardService } from './admin-guard.service';
import { BuildPackageComponent } from './build-package/build-package.component';
import { LoginComponent } from './login/login.component';
import { AdminComponent } from './admin/admin.component';
import { AboutComponent } from './about/about.component';
import { PackagePageComponent } from './package-page/package-page.component';
import { PackagesPageComponent } from './packages-page/packages-page.component';
import { BuildTaskPageComponent } from './build-task-page/build-task-page.component';
import { LoggedOutComponent } from './logged-out/logged-out.component';

const routes: Routes = [
  {
    path: '',
    canActivate: [AuthGuardService],
    component: BuildPackageComponent,
    children: []
  },
  {
    path: 'packages',
    canActivate: [AuthGuardService],
    component: PackagesPageComponent,
    children: []
  },
  {
    path: 'login',
    component: LoginComponent,
    children: []
  },
  {
    path: 'loggedout',
    component: LoggedOutComponent,
    children: []
  },
  {
    path: 'buildpackage',
    canActivate: [AuthGuardService],
    component: BuildPackageComponent,
    children: []
  },
  {
    path: 'admin',
    canActivate: [AdminGuardService],
    component: AdminComponent,
    children: []
  },
  {
    path: 'about',
    component: AboutComponent,
    children: []
  },
  {
    path: 'buildtask/:uniqueString',
    canActivate: [AuthGuardService],
    component: BuildTaskPageComponent,
  },
  { path: 'package/:name',
    canActivate: [AuthGuardService],
    component: PackagePageComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
