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

import it.cnr.istc.lecture.api.messages.Token;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.fx.ChartCanvas;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYDataItem;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonChartCanvas extends ChartCanvas {

    private final TokenXYSeries tokens = new TokenXYSeries("Tokens");

    public LessonChartCanvas() {
        super(ChartFactory.createScatterPlot(null, "", null, new XYSeriesCollection(), PlotOrientation.VERTICAL, false, true, false));
        XYSeriesCollection dataset = (XYSeriesCollection) ((XYPlot) getChart().getPlot()).getDataset();
        dataset.addSeries(tokens);
    }

    private class TokenXYSeries extends XYSeries {

        TokenXYSeries(Comparable key) {
            super(key);
        }

        public void add(double x, double y, Token t) {
            super.add(new TokenXYDataItem(x, y, t));
        }
    }

    private class TokenXYDataItem extends XYDataItem {

        private final Token t;

        TokenXYDataItem(double x, double y, Token t) {
            super(x, y);
            this.t = t;
        }
    }
}
