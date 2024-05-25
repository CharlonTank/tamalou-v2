import os
import subprocess


def check_toggling_state():
    file_path = "src/Backend.elm"
    search_line = "App.backend"

    with open(file_path, "r") as file:
        lines = file.readlines()

    for line in lines:
        if search_line in line:
            return line.strip().startswith("--")
    return False


def toggle_debugger_backend(new_token=None, toggling_on=False):
    file_path = "src/Backend.elm"
    search_line = "App.backend"
    additional_lines = 2
    lamdera_line = 3

    with open(file_path, "r") as file:
        lines = file.readlines()

    for i, line in enumerate(lines):
        if search_line in line:
            if toggling_on:
                lines[i] = line.replace("-- ", "", 1)
                for j in range(1, additional_lines + 1):
                    lines[i + j] = lines[i + j].replace("-- ", "", 1)
                if new_token:
                    lines[i + 2] = '        "{}"\n'.format(new_token)
                lines[i + lamdera_line] = (
                    "-- " + lines[i + lamdera_line]
                    if lines[i + lamdera_line].strip()
                    and not lines[i + lamdera_line].strip().startswith("--")
                    else lines[i + lamdera_line]
                )
            else:
                lines[i] = (
                    "-- " + line
                    if line.strip() and not line.strip().startswith("--")
                    else line
                )
                for j in range(1, additional_lines + 1):
                    lines[i + j] = (
                        "-- " + lines[i + j]
                        if lines[i + j].strip()
                        and not lines[i + j].strip().startswith("--")
                        else lines[i + j]
                    )
                lines[i + lamdera_line] = lines[i + lamdera_line].replace("-- ", "", 1)
            break

    with open(file_path, "w") as file:
        file.writelines(lines)

    subprocess.run(["elm-format", file_path, "--yes"])


def toggle_debugger_app(toggling_on):
    file_path = "src/Debuggy/App.elm"
    if toggling_on:
        start_line, end_line = 14, 87
        additional_line = 3
    else:
        start_line, end_line = 12, 86
        additional_line = 6

    with open(file_path, "r") as file:
        lines = file.readlines()

    if toggling_on:
        for i in range(start_line - 1, end_line):
            lines[i] = (
                lines[i].replace("-- ", "", 1)
                if lines[i].strip().startswith("--")
                else lines[i]
            )
        lines[additional_line - 1] = (
            lines[additional_line - 1].replace("-- ", "", 1)
            if lines[additional_line - 1].strip().startswith("--")
            else lines[additional_line - 1]
        )
    else:
        for i in range(start_line - 1, end_line):
            lines[i] = (
                "-- " + lines[i]
                if lines[i].strip() and not lines[i].strip().startswith("--")
                else lines[i]
            )
        lines[additional_line - 1] = (
            "-- " + lines[additional_line - 1]
            if lines[additional_line - 1].strip()
            and not lines[additional_line - 1].strip().startswith("--")
            else lines[additional_line - 1]
        )

    with open(file_path, "w") as file:
        file.writelines(lines)

    subprocess.run(["elm-format", file_path, "--yes"])


def main():
    toggling_on = check_toggling_state()
    if toggling_on:
        new_token = input("Enter new token (or press Enter to keep the old token): ")
        if new_token.strip() == "":
            new_token = None
        toggle_debugger_backend(new_token, toggling_on)
    else:
        toggle_debugger_backend(toggling_on=toggling_on)
    toggle_debugger_app(toggling_on)


if __name__ == "__main__":
    main()
