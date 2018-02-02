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
package it.cnr.istc.ale.client.context;

import com.fasterxml.jackson.core.JsonProcessingException;
import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.User;
import static it.cnr.istc.ale.client.context.Context.MAPPER;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import org.eclipse.paho.client.mqttv3.MqttException;

/**
 *
 * @author Riccardo De Benedictis
 */
public class UserContext {

    private static final Logger LOG = Logger.getLogger(UserContext.class.getName());
    private final Context ctx;
    /**
     * The current user.
     */
    final ObjectProperty<User> user = new SimpleObjectProperty<>();
    /**
     * The current user's parameter types.
     */
    final Map<String, Parameter> parameter_types = new HashMap<>();
    /**
     * The current user's parameter values.
     */
    final Map<String, Map<String, StringProperty>> parameter_values = new HashMap<>();
    /**
     * The current user's parameter values as a list, to be displayed on tables.
     * Notice that each parameter can aggregate more than a single value.
     */
    final ObservableList<ParameterValue> par_values = FXCollections.observableArrayList();

    UserContext(Context ctx) {
        this.ctx = ctx;
        // everytime a parameter value is updated, the update is broadcasted for the listening teachers.
        par_values.addListener((ListChangeListener.Change<? extends ParameterValue> c) -> {
            while (c.next()) {
                for (ParameterValue par_val : c.getAddedSubList()) {
                    par_val.value.addListener((ObservableValue<? extends String> observable, String oldValue, String newValue) -> {
                        Map<String, String> val = new HashMap<>();
                        for (Map.Entry<String, StringProperty> v_val : parameter_values.get(par_val.name.get()).entrySet()) {
                            val.put(v_val.getKey(), v_val.getValue().get());
                        }
                        try {
                            ctx.mqtt.publish(user.get().getId() + "/output/" + par_val.name.get(), MAPPER.writeValueAsBytes(val), 1, true);
                        } catch (JsonProcessingException | MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    });
                }
            }
        });
    }

    public ObjectProperty<User> getUser() {
        return user;
    }

    public ObservableList<ParameterValue> getParameterValues() {
        return par_values;
    }

    public static class ParameterValue {

        private final StringProperty name;
        private final StringProperty value;

        ParameterValue(String name, StringProperty value) {
            this.name = new SimpleStringProperty(name);
            this.value = value;
        }

        public StringProperty nameProperty() {
            return name;
        }

        public StringProperty valueProperty() {
            return value;
        }
    }
}
