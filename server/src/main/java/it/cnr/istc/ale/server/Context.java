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
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.messages.NewStudent;
import it.cnr.istc.ale.api.messages.LostStudent;
import it.cnr.istc.ale.api.messages.LostParameter;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.NewParameter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
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
    public static final String SERVER_ID = "server";
    private static Context context;
    private static final ObjectMapper MAPPER = new ObjectMapper();

    public static Context getContext() {
        if (context == null) {
            context = new Context();
        }
        return context;
    }
    private String host;
    private String service_port;
    private String mqtt_port;
    private MqttClient mqtt;
    private final Map<Long, Map<String, Parameter>> parameter_types = new HashMap<>();
    private final Map<Long, Map<String, Map<String, String>>> parameter_values = new HashMap<>();

    private Context() {
        try {
            Map<String, String> config = MAPPER.readValue(getClass().getResourceAsStream("/config.json"), new TypeReference<Map<String, String>>() {
            });
            host = config.get("host");
            service_port = config.get("service-port");
            mqtt_port = config.get("mqtt-port");
            mqtt = new MqttClient("tcp://" + host + ":" + mqtt_port, SERVER_ID, new MemoryPersistence());
            mqtt.setCallback(new MqttCallback() {
                @Override
                public void connectionLost(Throwable cause) {
                    LOG.log(Level.SEVERE, null, cause);
                }

                @Override
                public void messageArrived(String topic, MqttMessage message) throws Exception {
                }

                @Override
                public void deliveryComplete(IMqttDeliveryToken token) {
                }
            });
            MqttConnectOptions options = new MqttConnectOptions();
            options.setCleanSession(false);
            options.setAutomaticReconnect(true);
            mqtt.connect(options);
        } catch (MqttException | IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public String getHost() {
        return host;
    }

    public String getServicePort() {
        return service_port;
    }

    public String getMqttPort() {
        return mqtt_port;
    }

    public void addConnection(long id) {
        if (!parameter_types.containsKey(id)) {
            parameter_types.put(id, new HashMap<>());
        }
        if (!parameter_values.containsKey(id)) {
            parameter_values.put(id, new HashMap<>());
        }
        try {
            mqtt.publish(id + "/output/on-line", Boolean.TRUE.toString().getBytes(), 1, true);
            mqtt.subscribe(id + "/output", (String topic, MqttMessage message) -> {
                Message m = MAPPER.readValue(message.getPayload(), Message.class);
                if (m instanceof NewParameter) {
                    NewParameter np = (NewParameter) m;
                    Parameter par = np.getParameter();
                    parameter_types.get(id).put(par.getName(), par);
                    mqtt.subscribe(id + "/output/" + par.getName(), (String par_topic, MqttMessage par_value) -> {
                        parameter_values.get(id).put(par.getName(), MAPPER.readValue(par_value.getPayload(), new TypeReference<Map<String, String>>() {
                        }));
                    });
                } else if (m instanceof LostParameter) {
                    LostParameter lp = (LostParameter) m;
                    mqtt.unsubscribe(id + "/output/" + lp.getName());
                    parameter_types.get(id).remove(lp.getName());
                    parameter_values.get(id).remove(lp.getName());
                }
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void removeConnection(long id) {
        try {
            for (Map.Entry<String, Parameter> par_type : parameter_types.get(id).entrySet()) {
                mqtt.unsubscribe(id + "/output/" + par_type.getKey());
            }
            mqtt.unsubscribe(id + "/output");
            mqtt.publish(id + "/output/on-line", Boolean.FALSE.toString().getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void add_teacher(long student_id, long teacher_id) {
        try {
            mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new NewStudent(student_id)), 1, false);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void remove_teacher(long student_id, long teacher_id) {
        try {
            mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new LostStudent(student_id)), 1, false);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public Map<String, Parameter> get_parameter_types(long user_id) {
        return parameter_types.get(user_id);
    }

    public boolean is_online(long user_id) {
        return parameter_types.containsKey(user_id);
    }
}
