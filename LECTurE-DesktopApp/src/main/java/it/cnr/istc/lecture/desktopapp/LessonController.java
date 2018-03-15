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
import it.cnr.istc.lecture.desktopapp.TeachingLessonContext.TokenRow;
import java.net.URL;
import java.util.ResourceBundle;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.util.StringConverter;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.fx.ChartCanvas;
import org.jfree.chart.plot.PlotOrientation;
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
    private TableView<TokenRow> tokens_table_view;
    private TableColumn<TokenRow, Long> time_column;
    private TableColumn<TokenRow, Long> min_column;
    private TableColumn<TokenRow, Long> max_column;
    private TableColumn<TokenRow, String> id_column;
    private TableColumn<TokenRow, String> role_column;
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
            c.getAddedSubList().forEach(tk_row -> tokens.add(new TokenXYDataItem(tk_row.getTime(), 0, tk_row)));
            for (TokenRow tk_row : c.getRemoved()) {
                for (int i = 0; i < tokens.getItemCount(); i++) {
                    if (((TokenXYDataItem) tokens.getDataItem(i)).t == tk_row) {
                        tokens.remove(i);
                    }
                    break;
                }
            }
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
                newValue.tokensProperty().forEach(tk_row -> tokens.add(new TokenXYDataItem(tk_row.getTime(), 0, tk_row)));
                newValue.tokensProperty().addListener(TOKENS_CHANGE_LISTENER);
            } else {
                lesson_name.setText(null);
                play_button.setDisable(false);
                pause_button.setDisable(false);
                stop_button.setDisable(false);
                time.setText(null);
                lesson_chart_pane.getChildren().clear();
            }
        });

        play_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PLAY).color(Color.INDIGO));
        pause_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PAUSE).color(Color.INDIGO));
        stop_button.setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.STOP).color(Color.INDIGO));

        XYSeriesCollection series_collection = new XYSeriesCollection();
        series_collection.addSeries(tokens);
        lesson_chart_pane.getChildren().add(new ChartCanvas(ChartFactory.createScatterPlot(null, "", null, series_collection, PlotOrientation.VERTICAL, false, true, false)));
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
}
