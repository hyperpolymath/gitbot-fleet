// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//
// Smee.io client for local webhook testing
// Forwards GitHub webhooks from smee.io to local server

const SMEE_URL = Deno.env.get("SMEE_URL") || "https://smee.io/wT7QTqKbxTrez2V2";
const LOCAL_URL = Deno.env.get("LOCAL_URL") || "http://localhost:3000";

console.log(`🔗 Connecting to smee.io: ${SMEE_URL}`);
console.log(`📍 Forwarding to: ${LOCAL_URL}`);

async function connectToSmee() {
  const eventSourceUrl = SMEE_URL;

  const response = await fetch(eventSourceUrl, {
    headers: {
      "Accept": "text/event-stream",
    },
  });

  if (!response.ok) {
    throw new Error(`Failed to connect to smee: ${response.status}`);
  }

  const reader = response.body?.getReader();
  if (!reader) {
    throw new Error("No response body");
  }

  const decoder = new TextDecoder();
  let buffer = "";

  console.log("✅ Connected to smee.io, waiting for webhooks...\n");

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });
    const lines = buffer.split("\n");
    buffer = lines.pop() || "";

    for (const line of lines) {
      if (line.startsWith("data: ")) {
        try {
          const data = JSON.parse(line.slice(6));
          await forwardWebhook(data);
        } catch (e) {
          // Ignore ping/heartbeat messages
          if (!line.includes("ping")) {
            console.error("Failed to parse:", e);
          }
        }
      }
    }
  }
}

async function forwardWebhook(data) {
  const headers = new Headers();

  // Forward GitHub headers
  if (data["x-github-event"]) {
    headers.set("x-github-event", data["x-github-event"]);
  }
  if (data["x-github-delivery"]) {
    headers.set("x-github-delivery", data["x-github-delivery"]);
  }
  if (data["x-hub-signature-256"]) {
    headers.set("x-hub-signature-256", data["x-hub-signature-256"]);
  }
  headers.set("content-type", "application/json");

  // The actual payload is in data.body
  const body = data.body || data;

  console.log(`📨 Received: ${data["x-github-event"]} event`);

  try {
    const response = await fetch(LOCAL_URL, {
      method: "POST",
      headers,
      body: JSON.stringify(body),
    });

    console.log(`   ↳ Forwarded to local: ${response.status} ${response.statusText}`);
  } catch (e) {
    console.error(`   ↳ Failed to forward: ${e}`);
  }
}

// Reconnect on failure
while (true) {
  try {
    await connectToSmee();
  } catch (e) {
    console.error("Connection lost, reconnecting in 5s...", e);
    await new Promise((r) => setTimeout(r, 5000));
  }
}
