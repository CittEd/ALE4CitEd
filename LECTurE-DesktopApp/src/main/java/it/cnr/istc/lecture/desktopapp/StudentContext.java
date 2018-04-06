/*
 * Copyright (C) 2018 ISTC - CNR
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
package it.cnr.istc.lecture.desktopapp;

import it.cnr.istc.lecture.api.Parameter;
import it.cnr.istc.lecture.api.User;
import java.util.HashMap;
import java.util.Map;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

/**
 *
 * @author Riccardo De Benedictis
 */
public class StudentContext {

    private final User student;
    private final BooleanProperty on_line = new SimpleBooleanProperty();
    /**
     * The current student's parameter types.
     */
    private final ObservableList<Parameter> par_types = FXCollections.observableArrayList();
    private final Map<String, Parameter> id_par_types = new HashMap<>();
    /**
     * The current student's parameter values.
     */
    private final Map<String, Map<String, StringProperty>> par_vals = new HashMap<>();
    /**
     * The current student's parameter values as a list, to be displayed on
     * tables. Notice that each parameter can aggregate more than a single
     * value.
     */
    private final ObservableList<Context.ParameterValue> par_values = FXCollections.observableArrayList();

    StudentContext(User student) {
        this.student = student;
        par_types.addListener((ListChangeListener.Change<? extends Parameter> c) -> {
            while (c.next()) {
                for (Parameter par : c.getAddedSubList()) {
                    id_par_types.put(par.name, par);
                }
                for (Parameter par : c.getRemoved()) {
                    id_par_types.remove(par.name);
                }
            }
        });
    }

    public User getStudent() {
        return student;
    }

    public boolean isOnline() {
        return on_line.get();
    }

    public BooleanProperty onlineProperty() {
        return on_line;
    }

    public ObservableList<Parameter> parameterTypesProperty() {
        return par_types;
    }

    public Parameter getParameter(String par_name) {
        return id_par_types.get(par_name);
    }

    public void setParameterValue(String par_name, Map<String, String> values) {
        Map<String, StringProperty> c_vals = par_vals.computeIfAbsent(par_name, name -> new HashMap<>());
        for (Map.Entry<String, String> val : values.entrySet()) {
            if (c_vals.containsKey(val.getKey())) {
                c_vals.get(val.getKey()).set(val.getValue());
            } else {
                SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                c_vals.put(val.getKey(), val_prop);
                par_values.add(new Context.ParameterValue(par_name + "." + val.getKey(), val_prop));
            }
        }
    }

    public ObservableList<Context.ParameterValue> parametersProperty() {
        return par_values;
    }
}
