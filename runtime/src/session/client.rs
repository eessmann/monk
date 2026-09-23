//! ABI-2 client encoders and isolated native helper entrypoints.
use super::*;
fn publish(private_reply: bool, value: &[u8]) -> io::Result<()> {
    if private_reply {
        std::fs::write(capsule::path(&variable("MONK_SESSION_REPLY")?), value)
    } else {
        native::write_all(native::inherited_fd(1)?.as_fd(), value)
    }
}
pub fn client(private_reply: bool) -> io::Result<i32> {
    let action = (|| {
        if private_reply {
            publish(true, b"")?;
        }
        let socket = variable("MONK_SESSION_SOCKET")?;
        let token = variable("MONK_SESSION_TOKEN")?;
        let input = transport::read_all(native::inherited_fd(0)?.as_fd())?;
        let frames = protocol::decode(&input).map_err(|_| invalid("invalid session request"))?;
        let [operation, mask, operands @ ..] = frames.as_slice() else {
            return Err(invalid("invalid session request"));
        };
        let mask_value = u8::try_from(integer(mask)?)
            .ok()
            .and_then(|n| crate::types::DescriptorMask::new(n).ok())
            .ok_or_else(|| invalid("invalid descriptor mask"))?;
        let mut descriptors = Vec::new();
        for n in 0..3 {
            if mask_value.contains(source(n)?) {
                if !native::initial_descriptor_open(n + 3) {
                    return Err(invalid("missing user stream"));
                }
                descriptors.push(native::inherited_fd(n + 3)?);
            }
        }
        let mut payload = vec![token, operation.clone(), mask.clone()];
        if [opcode::RUN, opcode::SPAWN, opcode::SUBSTITUTION].contains(&operation.as_slice()) {
            let env = clean_environment(native::environment());
            payload.extend([Vec::new(), decimal(env.len())]);
            payload.extend(environment_frames(&env));
        } else if operation == opcode::FD_OPEN {
            payload.push(Vec::new());
        }
        payload.extend_from_slice(operands);
        if [
            opcode::RUN,
            opcode::SPAWN,
            opcode::SUBSTITUTION,
            opcode::FD_OPEN,
        ]
        .contains(&operation.as_slice())
        {
            descriptors.push(native::open_working_directory()?.into_owned());
        }
        transport::request(
            &socket,
            &descriptors.iter().map(AsFd::as_fd).collect::<Vec<_>>(),
            &protocol::encode(&payload),
        )
    })();
    publish(private_reply, &action.unwrap_or_else(|_| reply(125, None)))?;
    Ok(0)
}
pub fn child(capture: bool, request: crate::child::Request) -> io::Result<(i32, Bytes)> {
    let socket = variable("MONK_SESSION_SOCKET")?;
    let token = variable("MONK_SESSION_TOKEN")?;
    let env = clean_environment(native::environment());
    let values = protocol::decode(&request.state).map_err(|_| invalid("invalid child state"))?;
    let mut frames = vec![
        token,
        if capture {
            opcode::CAPTURE.to_vec()
        } else {
            opcode::RUN.to_vec()
        },
        decimal(request.mask.get()),
        Vec::new(),
        decimal(env.len()),
    ];
    frames.extend(environment_frames(&env));
    if capture {
        frames.push(request.warning);
    }
    frames.extend([
        body_opcode::SNAPSHOT.to_vec(),
        request.script,
        request.level,
    ]);
    frames.extend(values);
    let mut descriptors = Vec::new();
    for n in 0..3 {
        if request.mask.contains(source(n)?) {
            descriptors.push(native::inherited_fd(if n == 0 { 3 } else { n })?);
        }
    }
    descriptors.push(native::open_working_directory()?.into_owned());
    let response = transport::request(
        &socket,
        &descriptors.iter().map(AsFd::as_fd).collect::<Vec<_>>(),
        &protocol::encode(&frames),
    )?;
    match protocol::decode(&response)
        .map_err(|_| invalid("invalid child response"))?
        .as_slice()
    {
        [ok, status, value] if ok == b"ok" => Ok((
            integer(status)? as i32,
            if capture { value.clone() } else { Vec::new() },
        )),
        _ => Err(invalid("invalid child session reply")),
    }
}
pub fn directory_diagnostic() -> io::Result<i32> {
    let input = transport::read_all(native::inherited_fd(0)?.as_fd())?;
    let (origin, line, name, message) = crate::directory::diagnostic(&input)
        .map_err(|_| invalid("invalid directory diagnostic"))?;
    if message.is_empty() {
        return Ok(0);
    }
    let socket = variable("MONK_SESSION_SOCKET")?;
    let token = variable("MONK_SESSION_TOKEN")?;
    let env = clean_environment(native::environment());
    let error_open = native::initial_descriptor_open(2);
    let mut frames = vec![
        token,
        opcode::RUN.to_vec(),
        if error_open {
            b"4".to_vec()
        } else {
            b"0".to_vec()
        },
        Vec::new(),
        decimal(env.len()),
    ];
    frames.extend(environment_frames(&env));
    frames.extend([
        body_opcode::DIRECTORY_OUTPUT.to_vec(),
        origin,
        line,
        name,
        b"2".to_vec(),
        message,
    ]);
    let mut descriptors = Vec::new();
    if error_open {
        descriptors.push(native::inherited_fd(2)?);
    }
    descriptors.push(native::open_working_directory()?.into_owned());
    let response = transport::request(
        &socket,
        &descriptors.iter().map(AsFd::as_fd).collect::<Vec<_>>(),
        &protocol::encode(&frames),
    )?;
    match protocol::decode(&response)
        .map_err(|_| invalid("invalid directory session reply"))?
        .as_slice()
    {
        [ok, status, _] if ok == b"ok" => Ok(integer(status)? as i32),
        _ => Err(invalid("invalid directory session response")),
    }
}
pub fn writer(args: &[Bytes]) -> io::Result<i32> {
    let [directory, path, origin, line, name] = args else {
        return Err(invalid("invalid native writer arguments"));
    };
    let result = (|| {
        let file = std::fs::File::open(capsule::path(path))?;
        std::fs::remove_file(capsule::path(path))?;
        std::fs::remove_dir(capsule::path(directory))?;
        let mut chunk = [0; 65536];
        loop {
            let n = rustix::io::read(&file, &mut chunk)?;
            if n == 0 {
                break;
            }
            if !native::initial_descriptor_open(1) {
                return Err(io::Error::from_raw_os_error(libc::EBADF));
            }
            native::write_all(native::inherited_fd(1)?.as_fd(), &chunk[..n])?;
        }
        Ok(())
    })();
    match result {
        Ok(()) => Ok(0),
        Err(error) => {
            if native::initial_descriptor_open(2) {
                let _ = native::write_all(
                    native::inherited_fd(2)?.as_fd(),
                    &[
                        origin.as_slice(),
                        b": line ",
                        line,
                        b": ",
                        name,
                        b": write error: ",
                        &native::native_error_message(&error),
                        b"\n",
                    ]
                    .concat(),
                );
            }
            Ok(1)
        }
    }
}
pub fn exec_error(args: &[Bytes]) -> io::Result<i32> {
    let [directory, path, code] = args else {
        return Err(invalid("invalid failed executable diagnostic arguments"));
    };
    let code = integer(code)? as i32;
    let file = std::fs::File::open(capsule::path(path))?;
    std::fs::remove_file(capsule::path(path))?;
    std::fs::remove_dir(capsule::path(directory))?;
    if native::initial_descriptor_open(2)
        && let Ok(value) = transport::read_all(file.as_fd())
    {
        let _ = native::write_all(native::inherited_fd(2)?.as_fd(), &value);
    }
    Ok(code)
}
