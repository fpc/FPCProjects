import { Component, OnInit } from '@angular/core';
import { CategoryService } from '../category.service';
import { Category } from '../category';

@Component({
  selector: 'app-categories',
  templateUrl: './categories.component.html',
  styleUrls: ['./categories.component.css']
})
export class CategoriesComponent implements OnInit {

  categoryList: Category[];
  newCategory: Category = null;

  constructor(
    private categoryService: CategoryService
  ) { };

  ngOnInit() {
    this.categoryService.getCategoryList()
      .subscribe(categoryList => { this.categoryList = categoryList } )
  }

  updateField(category: Category) {
    this.categoryService.updateCategoryName(category);
  }
  saveNewCategory() {
    this.categoryService.addCategory(this.newCategory);
    this.newCategory = null;
  }

  addCategory() {
    this.newCategory = new Category;

  }
}
