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

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;

/**
 *
 * @author Riccardo De Benedictis
 */
public class QuestionnaireSectionPane extends GridPane implements ChangeListener<Integer> {

    private final String title;
    private final Label[] items;
    private final Spinner[] spinners;
    private int c_amount = 0;

    public QuestionnaireSectionPane(String title, String... items) {
        this.title = title;
        this.items = new Label[items.length];
        this.spinners = new Spinner[items.length];
        for (int i = 0; i < items.length; i++) {
            this.items[i] = new Label(items[i]);
            add(this.items[i], 0, i);
            this.spinners[i] = new Spinner<>(new SpinnerValueFactory.IntegerSpinnerValueFactory(0, 10));
            this.spinners[i].getStyleClass().add(Spinner.STYLE_CLASS_ARROWS_ON_RIGHT_HORIZONTAL);
            this.spinners[i].valueProperty().addListener(this);
            this.spinners[i].setPrefWidth(100);
            add(this.spinners[i], 1, i);

            RowConstraints row_c = new RowConstraints();
            row_c.setPercentHeight(1d / items.length * 100);
            getRowConstraints().add(row_c);
        }

        setHgap(10);
        setVgap(10);

        ColumnConstraints column1 = new ColumnConstraints();
        column1.setHgrow(Priority.ALWAYS);
        ColumnConstraints column2 = new ColumnConstraints();
        column2.setHgrow(Priority.NEVER);
        getColumnConstraints().addAll(column1, column2);
        setPadding(new Insets(15, 12, 15, 12));
    }

    @Override
    public void changed(ObservableValue<? extends Integer> observable, Integer oldValue, Integer newValue) {
        c_amount -= oldValue;
        c_amount += newValue;
        if (c_amount == 10) {
            for (Spinner spinner : spinners) {
                ((SpinnerValueFactory.IntegerSpinnerValueFactory) spinner.valueFactoryProperty().get()).setMax((Integer) spinner.valueProperty().get());
            }
        } else {
            for (Spinner spinner : spinners) {
                ((SpinnerValueFactory.IntegerSpinnerValueFactory) spinner.valueFactoryProperty().get()).setMax(10);
            }
        }
    }
}
