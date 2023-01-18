import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { OeDocsComponent } from './components/oe-docs/oe-docs.component';
import { PreviewComponent } from './components/preview/preview.component';
import { OeMasterPageComponent } from './components/oe-master-page/oe-master-page.component';

const browserWindow = window || {};
// @ts-ignore
const browserWindowEnv = browserWindow.__env || {};

let routes: Routes = [];

if (browserWindowEnv.BASE_MODULE && (browserWindowEnv.BASE_MODULE === 'bisq' || browserWindowEnv.BASE_MODULE === 'liquid')) {
 // we can override routes here if it will affect anything
} else {
}
routes = [
  {
    path: '',
    component: OeMasterPageComponent,
    children: [
      {
        path: 'preview-page',
        component: PreviewComponent,
      },
      {
        path: 'docs',
        component: OeDocsComponent
      }
    ],
  },
  {
    path: '**',
    redirectTo: 'preview-page'
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
})
export class OeRoutingModule { }
