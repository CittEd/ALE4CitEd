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
import javafx.collections.ListChangeListener;
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
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.XYDataItem;
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

        private final Map<String, XYSeriesCollection> par_collections = new HashMap<>();
        private final Map<String, XYPlot> par_plots = new HashMap<>();
        private final ChartViewer viewer;

        private StudentChartContext(StudentContext ctx) {
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

            for (Parameter par : ctx.parameterTypesProperty()) {
                XYSeriesCollection collection = new XYSeriesCollection();
                par_collections.put(par.name, collection);
                par.properties.entrySet().forEach(c_par -> collection.addSeries(new XYSeries(c_par.getKey())));
                NumberAxis range_axis = new NumberAxis("");
                final XYItemRenderer renderer = new StandardXYItemRenderer();
                XYPlot c_plot = new XYPlot(collection, null, range_axis, renderer);
                par_plots.put(par.name, c_plot);
                plot.add(c_plot, 1);

                for (ParameterValue par_v : ctx.parametersProperty()) {
                    String[] par_name = par_v.nameProperty().get().split("\\.");
                    if (par_name[0].equals(par.name)) {
                        XYSeries series = par_collections.get(par_name[0]).getSeries(par_name[1]);
                        switch (ctx.getParameter(par_name[0]).properties.get(par_name[1])) {
                            case "numeric":
                                par_v.updatesProperty().forEach(update -> series.add(new XYDataItem(update.time, Double.parseDouble(update.new_value))));
                                par_v.updatesProperty().addListener((ListChangeListener.Change<? extends Context.ParUpdate> c) -> {
                                    while (c.next()) {
                                        c.getAddedSubList().forEach(update -> series.add(new XYDataItem(update.time, Double.parseDouble(update.new_value))));
                                    }
                                });
                                break;
                            default:
                                throw new AssertionError(par_name[0] + " " + ctx.getParameter(par_name[0]).properties.get(par_name[1]));
                        }
                    }
                }
            }

            ctx.parameterTypesProperty().addListener((ListChangeListener.Change<? extends Parameter> c) -> {
                while (c.next()) {
                    for (Parameter par : c.getAddedSubList()) {
                        XYSeriesCollection collection = new XYSeriesCollection();
                        par_collections.put(par.name, collection);
                        par.properties.entrySet().forEach(c_par -> collection.addSeries(new XYSeries(c_par.getKey())));
                        NumberAxis range_axis = new NumberAxis("");
                        final XYItemRenderer renderer = new StandardXYItemRenderer();
                        XYPlot c_plot = new XYPlot(collection, null, range_axis, renderer);
                        par_plots.put(par.name, c_plot);
                        plot.add(c_plot, 1);
                    }
                    for (Parameter par : c.getRemoved()) {
                        par_collections.remove(par.name);
                        XYPlot c_plot = par_plots.remove(par.name);
                        plot.remove(c_plot);
                    }
                }
            });

            ctx.parametersProperty().addListener((ListChangeListener.Change<? extends ParameterValue> c) -> {
                while (c.next()) {
                    for (ParameterValue new_par : c.getAddedSubList()) {
                        String[] par_name = new_par.nameProperty().get().split("\\.");
                        XYSeries series = par_collections.get(par_name[0]).getSeries(par_name[1]);
                        switch (ctx.getParameter(par_name[0]).properties.get(par_name[1])) {
                            case "numeric":
                                new_par.updatesProperty().forEach(update -> series.add(new XYDataItem(update.time, Double.parseDouble(update.new_value))));
                                new_par.updatesProperty().addListener((ListChangeListener.Change<? extends Context.ParUpdate> c_c) -> {
                                    while (c_c.next()) {
                                        c_c.getAddedSubList().forEach(update -> series.add(new XYDataItem(update.time, Double.parseDouble(update.new_value))));
                                    }
                                });
                                break;
                            default:
                                throw new AssertionError(par_name[0] + " " + ctx.getParameter(par_name[0]).properties.get(par_name[1]));
                        }
                    }
                }
            });
        }
    }
}
