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

import it.cnr.istc.lecture.api.Lesson.LessonState;
import it.cnr.istc.lecture.api.model.EventTemplate;
import it.cnr.istc.lecture.api.model.QuestionEventTemplate;
import it.cnr.istc.lecture.api.model.TextEventTemplate;
import it.cnr.istc.lecture.api.model.URLEventTemplate;
import it.cnr.istc.lecture.desktopapp.TeachingLessonContext.TokenRow;
import java.net.URL;
import java.util.ResourceBundle;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.util.StringConverter;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.fx.ChartViewer;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYShapeRenderer;
import org.jfree.chart.util.DefaultShadowGenerator;
import org.jfree.data.xy.XYDataItem;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonController implements Initializable {

    @FXML
    private TextField lesson_name;
    @FXML
    private Button play_button;
    @FXML
    private Button pause_button;
    @FXML
    private Button stop_button;
    @FXML
    private TextField time;
    @FXML
    private StackPane lesson_chart_pane;
    @FXML
    private TableView<TokenRow> tokens_table_view;
    @FXML
    private TableColumn<TokenRow, Long> time_column;
    @FXML
    private TableColumn<TokenRow, Long> min_column;
    @FXML
    private TableColumn<TokenRow, Long> max_column;
    @FXML
    private TableColumn<TokenRow, String> id_column;
    @FXML
    private TableColumn<TokenRow, String> role_column;
    @FXML
    private TableColumn<TokenRow, String> subject_column;
    private final ChangeListener<Number> TIME_LISTENER = (ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> time.setText(TIME_STRING_CONVERTER.toString(newValue.longValue()));
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
    private final ObjectProperty<TeachingLessonContext> l_ctx = new SimpleObjectProperty<>();
    private final TokenXYSeries tokens = new TokenXYSeries("Tokens");
    private final ListChangeListener<TokenRow> TOKENS_CHANGE_LISTENER = (ListChangeListener.Change<? extends TokenRow> c) -> {
        while (c.next()) {
            c.getAddedSubList().forEach(tk_row -> {
                long x = tk_row.getTime();
                long y = 0;
                for (int i = 0; i < tokens.getItemCount(); i++) {
                    if (tokens.getDataItem(i).getXValue() == x) {
                        y++;
                    }
                }
                tokens.add(new TokenXYDataItem(x, y, tk_row));
            });
            c.getRemoved().forEach(tk_row -> {
                for (int i = 0; i < tokens.getItemCount(); i++) {
                    if (((TokenXYDataItem) tokens.getDataItem(i)).t == tk_row) {
                        tokens.remove(i);
                    }
                    break;
                }
            });
        }
    };

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        l_ctx.addListener((ObservableValue<? extends TeachingLessonContext> observable, TeachingLessonContext oldValue, TeachingLessonContext newValue) -> {
            if (oldValue != null) {
                oldValue.timeProperty().removeListener(TIME_LISTENER);
                oldValue.stateProperty().removeListener(STATE_LISTENER);
                oldValue.tokensProperty().removeListener(TOKENS_CHANGE_LISTENER);
            }
            tokens.clear();
            if (newValue != null) {
                lesson_name.setText(newValue.getLesson().name);
                STATE_LISTENER.changed(newValue.stateProperty(), oldValue != null ? oldValue.stateProperty().getValue() : null, newValue.stateProperty().getValue());
                newValue.stateProperty().addListener(STATE_LISTENER);
                TIME_LISTENER.changed(newValue.timeProperty(), oldValue != null ? oldValue.timeProperty().getValue() : null, newValue.timeProperty().getValue());
                newValue.timeProperty().addListener(TIME_LISTENER);
                newValue.tokensProperty().forEach(tk_row -> {
                    long x = tk_row.getTime();
                    long y = 0;
                    for (int i = 0; i < tokens.getItemCount(); i++) {
                        if (tokens.getDataItem(i).getXValue() == x) {
                            y++;
                        }
                    }
                    tokens.add(new TokenXYDataItem(x, y, tk_row));
                });
                newValue.tokensProperty().addListener(TOKENS_CHANGE_LISTENER);
                tokens_table_view.setItems(new SortedList<>(newValue.tokensProperty(), (TokenRow r0, TokenRow r1) -> Long.compare(r0.getTime(), r1.getTime())));
            } else {
                lesson_name.setText(null);
                play_button.setDisable(true);
                pause_button.setDisable(true);
                stop_button.setDisable(true);
                time.setText(null);
            }
        });

        play_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PLAY).color(Color.INDIGO));
        play_button.setOnAction((ActionEvent event) -> Context.getContext().play(l_ctx.get().getLesson()));
        pause_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PAUSE).color(Color.INDIGO));
        pause_button.setOnAction((ActionEvent event) -> Context.getContext().pause(l_ctx.get().getLesson()));
        stop_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.STOP).color(Color.INDIGO));
        stop_button.setOnAction((ActionEvent event) -> Context.getContext().stop(l_ctx.get().getLesson()));

        XYSeriesCollection series_collection = new XYSeriesCollection();
        series_collection.addSeries(tokens);

        XYShapeRenderer renderer = new XYShapeRenderer();
        renderer.setSeriesPaint(0, java.awt.Color.blue);

        XYPlot plot = new XYPlot(series_collection, new NumberAxis(""), new NumberAxis(""), renderer);
        plot.setShadowGenerator(new DefaultShadowGenerator(5, java.awt.Color.black, 1, 2, -45));
        plot.getRangeAxis().setVisible(false);
//        plot.getRangeAxis().setUpperBound(2);
//        plot.getRangeAxis().setLowerBound(-1);
        plot.setNoDataMessage("No data");
        plot.setRangeGridlinesVisible(false);

        JFreeChart chart = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false);
        chart.setBackgroundPaint(java.awt.Color.WHITE);

        lesson_chart_pane.getChildren().add(new ChartViewer(chart));

        time_column.setCellValueFactory(new PropertyValueFactory<>("time"));
        time_column.setEditable(true);
        time_column.setCellFactory((TableColumn<TokenRow, Long> param) -> new TimeTextFieldTableCell());
        time_column.setOnEditCommit((TableColumn.CellEditEvent<TokenRow, Long> event) -> {
            Context.getContext().setTime(l_ctx.get().getLesson(), event.getRowValue(), event.getNewValue());
        });
        min_column.setCellValueFactory(new PropertyValueFactory<>("min"));
        min_column.setCellFactory((TableColumn<TokenRow, Long> param) -> new TimeTextFieldTableCell());
        max_column.setCellValueFactory(new PropertyValueFactory<>("max"));
        max_column.setCellFactory((TableColumn<TokenRow, Long> param) -> new TimeTextFieldTableCell());

        id_column.setCellValueFactory(new PropertyValueFactory<>("name"));
        id_column.setCellFactory((TableColumn<TokenRow, String> param) -> new TableCell<TokenRow, String>() {
            @Override
            protected void updateItem(String item, boolean empty) {
                super.updateItem(item, empty);
                if (empty) {
                    setText(null);
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
                    styleProperty().unbind();
                    setStyle("");
                } else {
                    EventTemplate et = l_ctx.get().getModel().events.get(getIndex());
                    StudentContext student_ctx = Context.getContext().getStudentContext(l_ctx.get().getLesson().roles.get(et.role));
                    setText(et.role + " (" + student_ctx.getStudent().first_name + " " + student_ctx.getStudent().last_name + ")");

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
                    styleProperty().unbind();
                    setStyle("");
                } else {
                    EventTemplate et = l_ctx.get().getModel().events.get(getIndex());
                    if (et instanceof TextEventTemplate) {
                        setText(((TextEventTemplate) et).content);
                    } else if (et instanceof URLEventTemplate) {
                        setText(((URLEventTemplate) et).url);
                    } else if (et instanceof QuestionEventTemplate) {
                        setText(((QuestionEventTemplate) et).question);
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

        tokens_table_view.setRowFactory((TableView<TokenRow> param) -> {
            TableRow<TokenRow> row = new TableRow<>();
            row.itemProperty().addListener((ObservableValue<? extends TokenRow> observable, TokenRow oldValue, TokenRow newValue) -> {
                if (newValue == null) {
                    row.setContextMenu(null);
                } else {
                    row.setContextMenu(new NavigateContextMenu(row));
                }
            });
            return row;
        });
    }

    public ObjectProperty<TeachingLessonContext> teachingLessonContextProperty() {
        return l_ctx;
    }

    private class TokenXYSeries extends XYSeries {

        TokenXYSeries(Comparable key) {
            super(key);
        }

        public void add(double x, double y, TokenRow t) {
            super.add(new TokenXYDataItem(x, y, t));
        }
    }

    private class TokenXYDataItem extends XYDataItem {

        private final TokenRow t;

        TokenXYDataItem(double x, double y, TokenRow t) {
            super(x, y);
            this.t = t;
        }
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

    private class NavigateContextMenu extends ContextMenu {

        private final MenuItem go_to = new MenuItem("Go to", new Glyph("FontAwesome", FontAwesome.Glyph.SHARE));
        private final MenuItem edit = new MenuItem("Edit time", new Glyph("FontAwesome", FontAwesome.Glyph.EDIT));

        public NavigateContextMenu(TableRow<TokenRow> row) {
            go_to.setOnAction((ActionEvent event) -> {
                Context.getContext().goTo(l_ctx.get().getLesson(), row.getItem().getTime());
            });
            edit.setOnAction((ActionEvent event) -> {
                tokens_table_view.edit(row.getIndex(), tokens_table_view.getColumns().get(0));
            });
            edit.disableProperty().bind(row.getItem().executedProperty());
            getItems().addAll(go_to, new SeparatorMenuItem(), edit);
        }
    }
}
