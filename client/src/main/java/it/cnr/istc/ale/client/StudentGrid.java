/*
 * Copyright (C) 2017 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.ale.client;

import it.cnr.istc.ale.api.User;
import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class StudentGrid extends GridPane {

    private final TextField first_name = new TextField();
    private final TextField last_name = new TextField();
    private final TableView<Context.ParameterValue> par_values = new TableView<>();
    private final TableColumn<Context.ParameterValue, String> par_names = new TableColumn<>("Name");
    private final TableColumn<Context.ParameterValue, String> par_vals = new TableColumn<>("Value");

    public StudentGrid() {
        setHgap(10);
        setVgap(10);
        setPadding(new Insets(10));
        setHgrow(first_name, Priority.ALWAYS);
        setHgrow(last_name, Priority.ALWAYS);
        setVgrow(par_values, Priority.ALWAYS);
        setHgrow(par_values, Priority.ALWAYS);

        add(new Label("First name:"), 0, 0);
        first_name.setPromptText("First name");
        add(first_name, 1, 0);
        add(new Label("Last name:"), 0, 1);
        last_name.setPromptText("Last name");
        add(last_name, 1, 1);

        add(par_values, 0, 2, 2, 1);
        par_values.getColumns().addAll(par_names, par_vals);

        par_values.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
        par_names.setCellValueFactory(new PropertyValueFactory("name"));
        par_vals.setCellValueFactory(new PropertyValueFactory("value"));
    }

    public void setUser(User user) {
        first_name.setText(user.getFirstName());
        last_name.setText(user.getLastName());
        par_values.setItems(Context.getContext().getParameterValues(user.getId()));
    }
}
