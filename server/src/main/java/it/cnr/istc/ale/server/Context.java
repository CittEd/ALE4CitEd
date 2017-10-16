/*
 * Copyright (C) 2017 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
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
package it.cnr.istc.ale.server;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.cnr.istc.ale.api.messages.NewConnection;
import it.cnr.istc.ale.api.messages.LostConnection;
import it.cnr.istc.ale.api.messages.LostParameter;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.NewParameter;
import it.cnr.istc.ale.api.messages.ParameterUpdate;
import it.cnr.istc.ale.api.messages.UserOffline;
import it.cnr.istc.ale.api.messages.UserOnline;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class Context {

    private static final Logger LOG = Logger.getLogger(Context.class.getName());
    public static final String HOST = "localhost";
    public static final String SERVICE_PORT = "8080";
    public static final String MQTT_PORT = "1883";
    public static final String SERVER_ID = "server";
    private static Context context;
    private static final ObjectMapper mapper = new ObjectMapper();

    public static Context getContext() {
        if (context == null) {
            context = new Context();
        }
        return context;
    }
    private MqttClient mqtt;
    private final Map<Long, Map<String, NewParameter>> parameter_types = new HashMap<>();
    private final Map<Long, Map<String, ParameterUpdate>> parameter_values = new HashMap<>();

    private Context() {
        try {
            mqtt = new MqttClient("tcp://" + HOST + ":" + MQTT_PORT, SERVER_ID, new MemoryPersistence());
            MqttConnectOptions options = new MqttConnectOptions();
            options.setCleanSession(false);
            options.setAutomaticReconnect(true);
            mqtt.connect(options);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void addConnection(long id) {
        if (!parameter_types.containsKey(id)) {
            parameter_types.put(id, new HashMap<>());
        }
        if (!parameter_values.containsKey(id)) {
            parameter_values.put(id, new HashMap<>());
        }
        try {
            mqtt.publish(id + "/output", mapper.writeValueAsBytes(new UserOnline()), 2, false);
            mqtt.subscribe(id + "/output", (String topic, MqttMessage message) -> {
                LOG.log(Level.INFO, "New message: {0}", message);
                Message m = mapper.readValue(message.getPayload(), Message.class);
                if (m instanceof NewParameter) {
                    NewParameter np = (NewParameter) m;
                    parameter_types.get(id).put(np.getName(), np);
                } else if (m instanceof LostParameter) {
                    LostParameter lp = (LostParameter) m;
                    parameter_types.get(id).remove(lp.getName());
                    parameter_values.get(id).remove(lp.getName());
                } else if (m instanceof ParameterUpdate) {
                    ParameterUpdate pu = (ParameterUpdate) m;
                    parameter_values.get(id).put(pu.getParameter(), pu);
                } else {
                    LOG.log(Level.WARNING, "Not supported yet.. {0}", new String(message.getPayload()));
                }
            });
        } catch (MqttException | JsonProcessingException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void removeConnection(long id) {
        parameter_types.remove(id);
        parameter_values.remove(id);
        try {
            mqtt.unsubscribe(id + "/output");
            mqtt.publish(id + "/output", mapper.writeValueAsBytes(new UserOffline()), 2, false);
        } catch (MqttException | JsonProcessingException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void add_teacher(long user_id, long teacher_id) {
        try {
            mqtt.publish(String.valueOf(teacher_id) + "/input", mapper.writeValueAsBytes(new NewConnection(user_id, teacher_id)), 2, false);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void remove_teacher(long user_id, long teacher_id) {
        try {
            mqtt.publish(String.valueOf(teacher_id) + "/input", mapper.writeValueAsBytes(new LostConnection(user_id, teacher_id)), 2, false);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public Map<String, NewParameter> get_parameter_types(long user_id) {
        return parameter_types.get(user_id);
    }

    public Map<String, ParameterUpdate> get_parameter_values(long user_id) {
        return parameter_values.get(user_id);
    }

    public boolean is_online(long user_id) {
        return parameter_types.containsKey(user_id);
    }
}
