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

import it.cnr.istc.lecture.desktopapp.Context.ParameterValue;
import java.net.URL;
import java.text.FieldPosition;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.ResourceBundle;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.StackPane;
import javafx.util.StringConverter;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.fx.ChartViewer;
import org.jfree.chart.plot.CombinedDomainXYPlot;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author Riccardo De Benedictis
 */
public class StudentController implements Initializable {

    @FXML
    private TextField first_name;
    @FXML
    private TextField last_name;
    @FXML
    private TableView<ParameterValue> parameters_table_view;
    @FXML
    private TableColumn<ParameterValue, String> name_column;
    @FXML
    private TableColumn<ParameterValue, String> value_column;
    @FXML
    private StackPane student_chart_pane;
    private final ObjectProperty<StudentContext> std_ctx = new SimpleObjectProperty<>();
    private final Map<StudentContext, StudentChartContext> std_ctxs = new IdentityHashMap<>();
    private static final StringConverter TIME_STRING_CONVERTER = new TimeStringConverter();

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        name_column.setCellValueFactory(new PropertyValueFactory<>("name"));
        value_column.setCellValueFactory(new PropertyValueFactory<>("value"));

        std_ctx.addListener((ObservableValue<? extends StudentContext> observable, StudentContext oldValue, StudentContext newValue) -> {
            student_chart_pane.getChildren().clear();
            if (newValue != null) {
                first_name.setText(newValue.getStudent().first_name);
                last_name.setText(newValue.getStudent().last_name);
                parameters_table_view.setItems(newValue.parametersProperty());
                student_chart_pane.getChildren().add(std_ctxs.computeIfAbsent(newValue, ctx -> new StudentChartContext(ctx)).viewer);
            } else {
                first_name.setText(null);
                last_name.setText(null);
                parameters_table_view.setItems(null);
            }
        });
    }

    public ObjectProperty<StudentContext> studentContextProperty() {
        return std_ctx;
    }

    private class StudentChartContext {

        private final StudentContext ctx;
        private final Map<String, XYSeriesCollection> par_collections = new HashMap<>();
        private final Map<String, Map<String, XYSeries>> par_series = new HashMap<>();
        private final Map<String, XYPlot> par_plot = new HashMap<>();
        private final ChartViewer viewer;

        private StudentChartContext(StudentContext ctx) {
            this.ctx = ctx;

            final NumberAxis domain_axis = new NumberAxis("");
            domain_axis.setNumberFormatOverride(new NumberFormat() {
                @Override
                public StringBuffer format(double number, StringBuffer toAppendTo, FieldPosition pos) {
                    return format((long) number, toAppendTo, pos);
                }

                @Override
                public StringBuffer format(long number, StringBuffer toAppendTo, FieldPosition pos) {
                    return toAppendTo.append(TIME_STRING_CONVERTER.toString(number));
                }

                @Override
                public Number parse(String source, ParsePosition parsePosition) {
                    throw new UnsupportedOperationException("Not supported yet.");
                }
            });
            CombinedDomainXYPlot plot = new CombinedDomainXYPlot(domain_axis);
            plot.setGap(10.0);

            JFreeChart chart = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false);
            chart.setBackgroundPaint(java.awt.Color.WHITE);
            this.viewer = new ChartViewer(chart);
        }
    }
}
