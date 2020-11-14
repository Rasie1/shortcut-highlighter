# Shortcut Highlighter

This is a system for highlighting keyboard shortcuts and system data on Razer Blade RGB keyboard

## Capabilities

- Efficient
- Real time effects
- Shows pre-defined shortcuts from i3
- Shows i3 workspaces status
- Shows current languages (actually, manages switching languages too)
- Faster animation when CPU speed is higher

## Prerequisites

You should have [openrazer](https://github.com/openrazer/openrazer) installed and running.

## Installation and Running

Shortcut highlighter consists of two services.

Run keyboard daemon (it listens to keypresses and turns them into events for `shortcut-highlighter` service)

```
sudo pip3 install keyboard
sudo mkdir /etc/rasiel
cd keyboard-daemon && sudo python3 keyboard-daemon.py
```

Then, run `shortcut-highlighter`:

```
stack build && stack exec shortcut-highlighter
```

## systemd services

I provided systemd services in `systemd` directory for your convenience. Don't forget to edit them to change paths to executables.

## Configuration

- You should edit `src/Devices/Razer.hs` and put your id into line:

```
deviceAddress = "/org/razer/device/BY1750A44000357"
```

Currently, it's id for Razer Blade Stealth Late 2018. To find your id, list dbus objects.

- Edit layout of highlighted keys in `src/Layout.hs`

- If you have keyboard other than Razer, you can write a substitution of `src/Devices/Razer.hs` file

- You also might want to remove i3 from the project if you don't use it