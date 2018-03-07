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
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
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
    private final Map<String, Parameter> par_types = new HashMap<>();
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
}
