use std::rc::Rc;
use std::collections::HashMap;
use scitools::restype::*;

pub struct Resource {
    pub id: ResourceID,
    pub data: Vec<u8>,
}

pub struct ResourceManager {
    path: String,
    cache: HashMap<ResourceID, Rc<Resource>>,
}

impl ResourceManager {
    pub fn new(path: String) -> Self {
        ResourceManager{ path, cache: HashMap::new() }
    }

    pub fn get(&mut self, rtype: ResourceType, num: u16) -> Rc<Resource> {
        let id = ResourceID{ rtype, num };
        let resource_path = &self.path;
        let entry = self.cache.entry(id).or_insert_with(|| {
            let path = format!("{}/{}.{:03}", resource_path, resource_type_to_str(id.rtype), id.num);
            let data = match std::fs::read(path) {
                Ok(data) => data,
                Err(_) => Vec::new()
            };
            Rc::new(Resource{ id, data })
        });
        entry.clone()
    }
}
