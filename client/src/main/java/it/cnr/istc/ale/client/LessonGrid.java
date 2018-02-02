/*
 * Copyright (C) 2018 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
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

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.client.context.Context;
import it.cnr.istc.ale.client.context.TeachingContext.EventRow;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonGrid extends GridPane {

    private Lesson lesson;
    private final TextField lesson_name = new TextField();
    private final Button play_button = new Button("", new ImageView(new Image(getClass().getResourceAsStream("/images/play.png"))));
    private final Button pause_button = new Button("", new ImageView(new Image(getClass().getResourceAsStream("/images/pause.png"))));
    private final Button stop_button = new Button("", new ImageView(new Image(getClass().getResourceAsStream("/images/stop.png"))));
    private final Label time_label = new Label("Time:");
    private final TextField relative_time = new TextField();
    private final ObservableList<EventRow> events = FXCollections.observableArrayList();
    private final TableView<EventRow> events_table_view = new TableView<>(events);
    private final TableColumn<EventRow, Long> time_column = new TableColumn<>("Time");
    private final TableColumn<EventRow, String> subject_column = new TableColumn<>("Subject");
    private final ChangeListener<Number> TIME_LISTENER = (ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> relative_time.setText(format(newValue.longValue()));

    @SuppressWarnings("unchecked")
    public LessonGrid() {
        setHgap(10);
        setVgap(10);
        setPadding(new Insets(10));
        setHgrow(lesson_name, Priority.ALWAYS);
        setHgrow(time_label, Priority.ALWAYS);
        setHalignment(time_label, HPos.RIGHT);
        setVgrow(events_table_view, Priority.ALWAYS);
        setHgrow(events_table_view, Priority.ALWAYS);

        lesson_name.setPromptText("Lesson name");
        lesson_name.setEditable(false);
        add(lesson_name, 0, 0, 5, 1);

        play_button.setOnAction((ActionEvent event) -> Context.getContext().getTeachingContext().start(lesson));
        add(play_button, 0, 1);
        pause_button.setOnAction((ActionEvent event) -> Context.getContext().getTeachingContext().pause(lesson));
        add(pause_button, 1, 1);
        stop_button.setOnAction((ActionEvent event) -> Context.getContext().getTeachingContext().stop(lesson));
        add(stop_button, 2, 1);

        add(time_label, 3, 1);

        relative_time.setPromptText("Time");
        relative_time.setEditable(false);
        relative_time.setAlignment(Pos.CENTER_RIGHT);
        relative_time.setText(format(0));
        add(relative_time, 4, 1);

        events_table_view.getColumns().addAll(time_column, subject_column);
        events_table_view.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
        time_column.setCellValueFactory(new PropertyValueFactory("time"));
        time_column.setCellFactory((TableColumn<EventRow, Long> param) -> {
            return new TableCell<EventRow, Long>() {
                @Override
                protected void updateItem(Long item, boolean empty) {
                    super.updateItem(item, empty);
                    if (!empty) {
                        setText(null);
                    } else {
                        setText(format(item));
                        if (item <= Context.getContext().getTeachingContext().getLessonTime(lesson).get()) {
                            setStyle("-fx-font-weight: bold;");
                        } else {
                            setStyle("-fx-font-weight: normal;");
                        }
                    }
                }
            };
        });
        subject_column.setCellValueFactory(new PropertyValueFactory("subject"));
        subject_column.setCellFactory((TableColumn<EventRow, String> param) -> new TableCell<EventRow, String>() {
            @Override
            protected void updateItem(String item, boolean empty) {
                super.updateItem(item, empty);
                if (!empty) {
                    EventRow row = getTableView().getItems().get(getIndex());
                    if (row.getTime() < Context.getContext().getTeachingContext().getLessonTime(lesson).get()) {
                        setStyle("-fx-font-weight: bold;");
                    } else {
                        setStyle("-fx-font-weight: normal;");
                    }
                }
            }
        });

        add(events_table_view, 0, 2, 5, 1);
    }

    public void setLesson(final Lesson lesson) {
        if (this.lesson != null) {
            Context.getContext().getTeachingContext().getLessonTime(lesson).removeListener(TIME_LISTENER);
        }
        this.lesson = lesson;
        lesson_name.setText(lesson.getName());
        Context.getContext().getTeachingContext().getLessonTime(lesson).addListener(TIME_LISTENER);
    }

    public static String format(final long time) {
        long second = (time / 1000) % 60;
        long minute = (time / (1000 * 60)) % 60;
        long hour = (time / (1000 * 60 * 60)) % 24;
        long days = (time / (1000 * 60 * 60 * 24));
        if (days == 0) {
            if (hour == 0) {
                return String.format("%02d:%02d", minute, second);
            } else {
                return String.format("%02d:%02d:%02d", hour, minute, second);
            }
        } else {
            return String.format("%03d:%02d:%02d:%02d", days, hour, minute, second);
        }
    }
}
