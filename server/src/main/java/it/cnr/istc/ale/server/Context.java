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
import it.cnr.istc.ale.api.messages.ConnectionCreation;
import it.cnr.istc.ale.api.messages.ConnectionDestruction;
import it.cnr.istc.ale.api.messages.Message;
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

    private Context() {
        try {
            mqtt = new MqttClient("tcp://" + HOST + ":" + MQTT_PORT, SERVER_ID, new MemoryPersistence());
            MqttConnectOptions options = new MqttConnectOptions();
            options.setCleanSession(false);
            mqtt.connect(options);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void addConnection(String id) {
        try {
            mqtt.subscribe(id + "/output", (String topic, MqttMessage message) -> {
                Message m = mapper.readValue(message.getPayload(), Message.class);
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void removeConnection(String id) {
        try {
            mqtt.unsubscribe(id);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void add_teacher(long user_id, long teacher_id) {
        try {
            mqtt.publish(String.valueOf(teacher_id) + "/input", new MqttMessage(mapper.writeValueAsBytes(new ConnectionCreation(user_id, teacher_id))));
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void remove_teacher(long user_id, long teacher_id) {
        try {
            mqtt.publish(String.valueOf(teacher_id) + "/input", new MqttMessage(mapper.writeValueAsBytes(new ConnectionDestruction(user_id, teacher_id))));
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }
}
