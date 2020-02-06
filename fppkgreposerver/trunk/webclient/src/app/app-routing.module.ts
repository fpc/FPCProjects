import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { AuthGuardService } from './auth-guard.service';
import { AdminGuardService } from './admin-guard.service';
import { BuildPackageComponent } from './build-package/build-package.component';
import { LoginComponent } from './login/login.component';
import { AdminComponent } from './admin/admin.component';
import { CategoriesComponent } from './categories/categories.component';
import { AboutComponent } from './about/about.component';
import { PackagePageComponent } from './package-page/package-page.component';
import { PackagesPageComponent } from './packages-page/packages-page.component';
import { BuildTaskPageComponent } from './build-task-page/build-task-page.component';
import { LoggedOutComponent } from './logged-out/logged-out.component';

const routes: Routes = [
  {
    path: 'build',
    canActivate: [AuthGuardService],
    component: BuildPackageComponent,
    children: []
  },
  {
    path: '',
    component: PackagesPageComponent,
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
    path: 'categories',
    canActivate: [AdminGuardService],
    component: CategoriesComponent,
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
  {
    path: 'forbidden',
    component: LoginComponent,
    children: []
  },
  {
    path: 'unauthorized',
    component: LoginComponent,
    children: []
  },
  { path: 'package/:name',
    canActivate: [AuthGuardService],
    component: PackagePageComponent
  },
  { path: 'package/:name/:version',
    canActivate: [AuthGuardService],
    component: PackagePageComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
