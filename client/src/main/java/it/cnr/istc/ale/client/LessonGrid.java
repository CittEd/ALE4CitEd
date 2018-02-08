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
import it.cnr.istc.ale.api.LessonState;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.model.EventTemplate;
import it.cnr.istc.ale.api.model.QuestionEventTemplate;
import it.cnr.istc.ale.api.model.TextEventTemplate;
import it.cnr.istc.ale.client.context.Context;
import it.cnr.istc.ale.client.context.TeachingContext.TokenRow;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.transformation.SortedList;
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
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.util.StringConverter;

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
    private final TableView<TokenRow> tokens_table_view = new TableView<>();
    private final TableColumn<TokenRow, Long> time_column = new TableColumn<>("Time");
    private final TableColumn<TokenRow, String> id_column = new TableColumn<>("ID");
    private final TableColumn<TokenRow, String> role_column = new TableColumn<>("Role");
    private final TableColumn<TokenRow, String> subject_column = new TableColumn<>("Subject");
    private final ChangeListener<Number> TIME_LISTENER = (ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> relative_time.setText(TIME_STRING_CONVERTER.toString(newValue.longValue()));
    private final ChangeListener<LessonState> STATE_LISTENER = (ObservableValue<? extends LessonState> observable, LessonState oldValue, LessonState newValue) -> {
        if (newValue != null) {
            switch (newValue) {
                case Running:
                    play_button.setDisable(true);
                    pause_button.setDisable(false);
                    stop_button.setDisable(false);
                    break;
                case Paused:
                    play_button.setDisable(false);
                    pause_button.setDisable(true);
                    stop_button.setDisable(false);
                    break;
                case Stopped:
                    play_button.setDisable(false);
                    pause_button.setDisable(true);
                    stop_button.setDisable(true);
                    break;
                default:
                    throw new AssertionError(newValue.name());
            }
        }
    };
    private static final StringConverter TIME_STRING_CONVERTER = new TimeStringConverter();

    @SuppressWarnings("unchecked")
    public LessonGrid() {
        setHgap(10);
        setVgap(10);
        setPadding(new Insets(10));
        setHgrow(lesson_name, Priority.ALWAYS);
        setHgrow(time_label, Priority.ALWAYS);
        setHalignment(time_label, HPos.RIGHT);
        setVgrow(tokens_table_view, Priority.ALWAYS);
        setHgrow(tokens_table_view, Priority.ALWAYS);

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
        relative_time.setText(TIME_STRING_CONVERTER.toString(0));
        add(relative_time, 4, 1);

        tokens_table_view.getColumns().addAll(time_column, id_column, role_column, subject_column);
        tokens_table_view.setEditable(true);
        time_column.prefWidthProperty().bind(tokens_table_view.widthProperty().multiply(0.2));
        id_column.prefWidthProperty().bind(tokens_table_view.widthProperty().multiply(0.2));
        role_column.prefWidthProperty().bind(tokens_table_view.widthProperty().multiply(0.2));
        subject_column.prefWidthProperty().bind(tokens_table_view.widthProperty().multiply(0.4));

        time_column.setCellValueFactory(new PropertyValueFactory<>("time"));
        time_column.setEditable(true);
        time_column.setCellFactory((TableColumn<TokenRow, Long> param) -> new TimeTextFieldTableCell());
        time_column.setOnEditCommit((TableColumn.CellEditEvent<TokenRow, Long> event) -> {
            Context.getContext().getTeachingContext().setTime(lesson, event.getRowValue(), event.getNewValue());
        });
        id_column.setCellValueFactory(new PropertyValueFactory<>("name"));
        id_column.setCellFactory((TableColumn<TokenRow, String> param) -> new TableCell<TokenRow, String>() {
            @Override
            protected void updateItem(String item, boolean empty) {
                super.updateItem(item, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                    styleProperty().unbind();
                    setStyle("");
                } else {
                    setText(item);

                    TokenRow row = getTableView().getItems().get(getIndex());
                    styleProperty().bind(Bindings.createStringBinding(() -> {
                        if (row.getExecuted()) {
                            return "-fx-font-weight: bold;";
                        } else {
                            return "";
                        }
                    }, row.executedProperty()));
                }
            }
        });
        role_column.setCellValueFactory(new PropertyValueFactory<>("name"));
        role_column.setCellFactory((TableColumn<TokenRow, String> param) -> new TableCell<TokenRow, String>() {
            @Override
            protected void updateItem(String item, boolean empty) {
                super.updateItem(item, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                    styleProperty().unbind();
                    setStyle("");
                } else {
                    EventTemplate et = Context.getContext().getTeachingContext().getLessonModel(lesson).getModel().stream().filter(templ -> templ.getName().equals(item)).findAny().get();
                    User student = Context.getContext().getTeachingContext().getStudent(lesson.getRoles().get(et.getRole()));
                    setText(et.getRole() + " (" + student.getFirstName() + " " + student.getLastName() + ")");

                    TokenRow row = getTableView().getItems().get(getIndex());
                    styleProperty().bind(Bindings.createStringBinding(() -> {
                        if (row.getExecuted()) {
                            return "-fx-font-weight: bold;";
                        } else {
                            return "";
                        }
                    }, row.executedProperty()));
                }
            }
        });
        subject_column.setCellValueFactory(new PropertyValueFactory<>("name"));
        subject_column.setCellFactory((TableColumn<TokenRow, String> param) -> new TableCell<TokenRow, String>() {
            @Override
            protected void updateItem(String item, boolean empty) {
                super.updateItem(item, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                    styleProperty().unbind();
                    setStyle("");
                } else {
                    EventTemplate et = Context.getContext().getTeachingContext().getLessonModel(lesson).getModel().stream().filter(templ -> templ.getName().equals(item)).findAny().get();
                    if (et instanceof TextEventTemplate) {
                        setText(((TextEventTemplate) et).getContent());
                    } else if (et instanceof QuestionEventTemplate) {
                        setText(((QuestionEventTemplate) et).getQuestion());
                    }

                    TokenRow row = getTableView().getItems().get(getIndex());
                    styleProperty().bind(Bindings.createStringBinding(() -> {
                        if (row.getExecuted()) {
                            return "-fx-font-weight: bold;";
                        } else {
                            return "";
                        }
                    }, row.executedProperty()));
                }
            }
        });

        add(tokens_table_view, 0, 2, 5, 1);
    }

    public void setLesson(final Lesson lesson) {
        if (this.lesson != null) {
            Context.getContext().getTeachingContext().getLessonTime(this.lesson).removeListener(TIME_LISTENER);
            Context.getContext().getTeachingContext().getLessonState(this.lesson).removeListener(STATE_LISTENER);
        }
        this.lesson = lesson;
        lesson_name.setText(lesson.getName());
        Context.getContext().getTeachingContext().getLessonTime(lesson).addListener(TIME_LISTENER);
        TIME_LISTENER.changed(Context.getContext().getTeachingContext().getLessonTime(lesson), null, Context.getContext().getTeachingContext().getLessonTime(lesson).get());
        Context.getContext().getTeachingContext().getLessonState(lesson).addListener(STATE_LISTENER);
        STATE_LISTENER.changed(Context.getContext().getTeachingContext().getLessonState(lesson), null, Context.getContext().getTeachingContext().getLessonState(lesson).get());
        tokens_table_view.setItems(new SortedList<>(Context.getContext().getTeachingContext().getTokens(lesson), (TokenRow r0, TokenRow r1) -> Long.compare(r0.getTime(), r1.getTime())));
    }

    private static class TimeTextFieldTableCell extends TextFieldTableCell<TokenRow, Long> {

        public TimeTextFieldTableCell() {
            super(TIME_STRING_CONVERTER);
        }

        @Override
        public void updateItem(Long item, boolean empty) {
            super.updateItem(item, empty);
            if (empty) {
                setText(null);
                setGraphic(null);
                editableProperty().unbind();
                styleProperty().unbind();
                setStyle("");
            } else {
                setText(TIME_STRING_CONVERTER.toString(item));

                TokenRow row = getTableView().getItems().get(getIndex());
                editableProperty().bind(row.executedProperty().not());
                styleProperty().bind(Bindings.createStringBinding(() -> {
                    if (row.getExecuted()) {
                        return "-fx-font-weight: bold;";
                    } else {
                        return "";
                    }
                }, row.executedProperty()));
            }
        }
    }
}
