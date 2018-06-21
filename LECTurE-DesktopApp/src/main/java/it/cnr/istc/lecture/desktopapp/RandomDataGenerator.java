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

import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

/**
 *
 * @author Riccardo De Benedictis
 */
public class RandomDataGenerator implements ChangeListener<Boolean> {

    private final Random random = new Random();
    private ScheduledExecutorService simulate_data_executor;

    @Override
    public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
        if (newValue) {
            simulate_data_executor = Executors.newSingleThreadScheduledExecutor();
            simulate_data_executor.scheduleAtFixedRate(() -> {
                Context.getContext().parametersProperty().forEach(par -> {
                    String[] par_name = par.nameProperty().get().split("\\.");
                    if (Context.getContext().getParameter(par_name[0]).properties.get(par_name[1]).equals("numeric")) {
                        double c_val = Double.parseDouble(par.valueProperty().get());
                        Context.getContext().setParameterValue(par_name[0], par_name[1], Double.toString(c_val + (random.nextBoolean() ? Math.max(1, random.nextDouble() * c_val / 100) : -Math.max(1, random.nextDouble() * c_val / 100))));
                    }
                });
            }, 0, 1, TimeUnit.SECONDS);
        } else {
            simulate_data_executor.shutdown();
        }
    }

    public void shutdown() {
        if (simulate_data_executor != null && !simulate_data_executor.isShutdown()) {
            simulate_data_executor.shutdown();
        }
    }
}
